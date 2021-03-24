{- | Module containing things concering compilation from Xp to C code output.

Doofus implementation, basically just concatenating strings.
-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Xp.Compile where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as St
import qualified Data.Map.Strict as M
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens.Mtl
import qualified Lens.Micro.TH as Lens.TH

import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Proxy

import Xp.Core hiding (freshId)


--
-- * Compilation utils
--

type Compile = R.ReaderT Name (St.State Cm)

newtype Name = Name
    { unName :: String
    } deriving newtype (Show)
      deriving stock (Eq, Ord)

-- | State during compilation.
data Cm = Cm
    { _cmCounter :: Int
    -- ^ Seed for generating unique variables.
    , _cmDefs    :: [String]
    -- ^ Function definitions added as necssary.
    , _cmGlobals :: [String]
    -- ^ Global variable declarations.
    , _cmStructs :: M.Map Name String
    -- ^ Definitions for structs. Maps name of struct (type) to definition.
    }
$(Lens.TH.makeLenses ''Cm)

-- ** Other utilities

-- | Return a unique identifier and increment the counter in state.
freshId :: Compile Name
freshId = do
    newId <- ('v' :) . show <$> Lens.Mtl.use cmCounter
    Lens.Mtl.modifying cmCounter (+ 1)
    pure $ Name newId

--
-- * Main compilation stuff
--

{- | Main entry point for compilation. Compile an 'Xp.Xp' program (expression)
into its C representation.
-}
compile :: Show a => Xp a -> String
compile program =
    let (code, st) :: (String, Cm) =
            St.runState (R.runReaderT (cXp program) initName) initCm
        main = mainWrap code
    in mconcat
        [ "\n// Code generated from Xp program \n\n"

        -- #include lines
        , concatMap ((++ "\n") . includeWrap)
            ["stdbool.h", "stdlib.h", "stdio.h"], "\n"

        -- Define structs
        , "// Structs representing product types\n"
        , concatMap (++ "\n") (st Lens.^. cmStructs), "\n"

        -- Declare global variables
        , "// Variables correpsonding to scrutinees in case expressions\n"
        , concatMap (++ "\n") (st Lens.^. cmGlobals), "\n"

        -- The definitions are added in the wrong order (for the C code),
        -- so we reverse the list of definitions before printing them.
        , concatMap (++ "\n") . reverse $ (st Lens.^. cmDefs)
        , main
        ]
  where
    initName :: Name
    initName = Name "scrut"

    initCm :: Cm
    initCm = Cm
        { _cmCounter = 0
        , _cmDefs    = []
        , _cmGlobals = []
        , _cmStructs = M.empty
        }

    includeWrap :: String -> String
    includeWrap s = "#include <" ++ s ++ ">"

    mainWrap :: String -> String
    mainWrap body = mconcat
        [ "int main() {\n"
        , "    int output = ", body, ";\n"
        , "    printf(\"Program output is: %d\\n\", output);\n"
        , "    return 0;\n"
        , "}\n"
        ]

{- | Compile an expression into its C representation. Needed function
definitions are generated as needed and added to the compilation state.
-}
cXp :: Show a => Xp a -> Compile String
cXp = \case
    Val v -> pure $ map toLower $ show v  -- Hack to get """C bool literals"""
    Var s -> pure $ s
    Add e1 e2 -> binOp "+" e1 e2
    Mul e1 e2 -> binOp "*" e1 e2
    Sub e1 e2 -> binOp "-" e1 e2
    Div e1 e2 -> binOp "/" e1 e2
    Gt  e1 e2 -> binOp ">" e1 e2
    Lt  e1 e2 -> binOp "<" e1 e2
    Eq  e1 e2 -> binOp "==" e1 e2
    And e1 e2 -> binOp "&&" e1 e2
    Or  e1 e2 -> binOp "||" e1 e2
    Not e -> ("(!" ++) . (++ ")") <$> cXp e

    IfThenElse cond eTrue eFalse -> do
        funName <- freshId
        Name resVar <- freshId
        eTrueStr <- cXp eTrue
        eFalseStr <- cXp eFalse
        let def = concat
                -- TODO: Hardcoded types (do typing)
                [ "int ", unName funName, "(int condition) {\n"
                , "    int ", resVar, ";\n"
                , "    if (condition) {\n"
                , "        ", resVar, " = ", eTrueStr, ";\n"
                , "    }\n"
                , "    else {\n"
                , "        ", resVar, " = ", eFalseStr, ";\n"
                , "    }\n"
                , "    return ", resVar, ";\n"
                , "}\n"
                ]
        Lens.Mtl.modifying cmDefs (def :)

        condStr <- cXp cond
        pure $ concat [unName funName, "(", condStr, ")"]

    Case (Scrut scrutName scrut) matches -> do
        funName <- freshId

        newCaseFun funName (scrutName, matches)
        scrutStr <- cXp scrut

        pure $ mconcat [unName funName, "(", scrutStr, ")"]

    Case2 (Proxy :: Proxy pt) (Scrut scrutName (transformee :: Xp t)) body -> do
        let sName = structName @pt

        -- Generate the definition of the struct if it's the first time
        -- encountered.
        existingStructs <- Lens.Mtl.use cmStructs
        unless (Name (sName) `M.member` existingStructs)
            (newStructDef @pt)

        -- Create function to transform value to struct!
        structReturnerFunName <- freshId
        newStructReturnerDef @t @pt structReturnerFunName
        transformeeStr <- cXp transformee
        let getStructCall = concat
                [unName structReturnerFunName, "(", transformeeStr, ")"]

        -- Create function for the pattern matching logic!
        newGlobalVar (sName ++ "*") scrutName
        funName <- freshId
        Name argName <- freshId
        Name resVar <- freshId
        bodyStr <- cXp body
        let def = concat
                -- Hardcoded type (TODO)
                [ "int ", unName funName, "(", sName, "* ", argName, ") {\n"
                , "    ", scrutName, " = ", argName, ";\n"
                , "    int ", resVar, " = ", bodyStr, ";\n"
                , "    return ", resVar, ";\n"
                , "}\n"
                ]
        Lens.Mtl.modifying cmDefs (def :)
        pure $ concat [unName funName, "(", getStructCall, ")"]

    SVar name -> pure name

    SFieldRef (FieldRef name idx :: FieldRef pt) -> do
        let (Field _ s _) = toFields (dummy @pt) !! idx
        pure $ concat [name, "->", s]

    Cast t e -> do
        eStr <- cXp e
        pure $ concat ["((", showTRep t, ") ", eStr, ")"]

  where
    binOp :: (Show a, Show b) => String -> Xp a -> Xp b -> Compile String
    binOp op e1 e2 = do
        s1 <- cXp e1
        s2 <- cXp e2
        pure $ mconcat ["(", s1, " ", op, " ", s2, ")"]

{- | Convert the body of a sum pattern match ('Xp.Case') to a new function
definition, and add it to the compilation state.
-}
newCaseFun :: forall b .
    ( Show b
    )
    => Name
    -- ^ Name of function.
    -> (String, [(Xp Bool, Xp b)])
    -- ^ Name of scrutinee, and branches/matches of case-expression.
    -> Compile ()
newCaseFun (Name funName) (scrutName, matches) = do
    resVar <- freshId
    matchesStr <- mapM (cMatch resVar) matches
    newGlobalVar "int" scrutName

    Name argName <- freshId

    let def = mconcat
            [ "int ", funName, "(int ", argName, ") {\n"
            , "    ", scrutName, " = ", argName, ";\n"
            , "    int ", unName resVar, ";\n"
            , concatMap ("    " ++) matchesStr
            -- Hacky thing to soak the final dangling "else"
            -- Printout should probably not be needed; we ideally want to
            -- perform the exhaustiveness check in Haskell instead. But it
            -- looks a bit empty otherwise.
            , "    { printf(\"Non-exhaustive conditions in function `",
                funName, "`\\n\"); }\n"
            , "    return ", unName resVar, ";\n"
            , "}\n"
            ]
    Lens.Mtl.modifying cmDefs (def :)
  where
    cMatch :: Name -> (Xp Bool, Xp b) -> Compile String
    cMatch (Name resVar) (cond, body) = do
        condStr <- cXp cond
        bodyStr <- cXp body
        pure $ mconcat ["if (", condStr, ") { ", resVar, " = ", bodyStr, "; } else\n"]

-- TODO: TYPE THINGS. IT IS HARD WHEN EVERYTHING IS A STRING.
{- | Add a global variable to the compilation state, given the name of the
variable's type, and the variable name.
-}
newGlobalVar ::
       String
    -- ^ Type of variable.
    -> String
    -- ^ Name of variable.
    -> Compile ()
newGlobalVar typeStr varStr =
    let def = concat [typeStr, " ", varStr, ";"]
    in Lens.Mtl.modifying cmGlobals (def :)

{- | Create a function which returns a pointer to a struct instance, and add
it to the compilation state.
-}
newStructReturnerDef :: forall t pt . ToStruct t pt
    => Name        -- ^ Name of function.
    -> Compile ()
newStructReturnerDef (Name funName) = do
    Name transformeeId <- freshId

    -- v This shouldn't be done here, do it before saving it in the AST.
    let struct = toStruct @t @pt (SVar transformeeId)
    let sName= structName @pt

    -- HARDCODED TYPE (double). Introduce types to the AST to fix (TODO).
    newGlobalVar "double" transformeeId

    Name resVar <- freshId
    insts <- mapM instField (toFields @pt struct)
    let argName = "arg"
    let def = concat
                -- TODO: again, hardcoded type
            [ sName, "* ", funName, "(double arg) {\n"
            , "    ", transformeeId, " = arg;\n"
            -- TODO: SET UP free() SOMEHOW SO WE DON'T GET MEMORY LEAKS HERE
            , "    ", sName, "* ", resVar, " = malloc(sizeof(", sName, "));\n"
            , concatMap (\ xs -> "    " ++ resVar ++ xs ++ ";\n") insts
            , "    return ", resVar, ";\n"
            , "}\n"
            ]
    Lens.Mtl.modifying cmDefs (def :)
  where
    instField :: Field -> Compile String
    instField (Field _ s v) = do
        vStr <- cXp v
        pure $ concat ["->", s, " = ", vStr]

-- | Create a struct definition and add it to the compilation state.
newStructDef :: forall pt . Struct pt => Compile ()
newStructDef = Lens.Mtl.modifying cmStructs (M.insert (Name nameStr) def)
  where
    nameStr :: String
    nameStr = structName @pt

    fieldsStr :: [String]
    fieldsStr = map showField $ toFields (dummy @pt)
      where
        showField :: Field -> String
        showField (Field t s _) = showTRep t ++ " " ++ s ++ ";\n"

    def :: String
    def = concat
            [ "typedef struct ", nameStr, " {\n"
            , concatMap ("    " ++) fieldsStr
            , "} ", nameStr, ";\n"
            ]
