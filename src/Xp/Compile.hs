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

import qualified Xp.Core as X

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Proxy (Proxy (Proxy))

import Xp.Core (Xp)


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
    -- ^ Definitions for structs. Maps name of struct (its type) to definition.
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
compile :: forall a . (X.CType a, Show a)
    => Xp a
    -> String
compile program =
    let (code, st) :: (String, Cm) =
            St.runState (R.runReaderT (cXp program) initName) initCm
        main = mainWrap @a code
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

    mainWrap :: forall retType . (X.CType retType)
        => String -> String
    mainWrap body = mconcat
        [ "int main() {\n"
        , "    ", X.cType @retType, " output = ", body, ";\n"
        , "    printf(\"Program output is: %",
                formatting (X.cType @retType) : "\\n\", output);\n"
        , "    return 0;\n"
        , "}\n"
        ]
      where
        -- Quickfix thing, this should probably be determined by type
        -- and not like this
        formatting :: String -> Char
        formatting = \case
            "double" -> 'f'
            "int" -> 'd'
            "bool" -> 'd'
            s -> error $ mconcat
                ["formatting: cannot format for C type `", s, "`"]

{- | Compile an expression into its C representation. Needed function
definitions are generated as needed and added to the compilation state.
-}
cXp :: Show a => Xp a -> Compile String
cXp = \case
    X.Val v -> pure $ map toLower $ show v  -- Hack to get """C bool literals"""
    X.Var s -> pure s
    X.Add e1 e2 -> binOp "+" e1 e2
    X.Mul e1 e2 -> binOp "*" e1 e2
    X.Sub e1 e2 -> binOp "-" e1 e2
    X.Div e1 e2 -> binOp "/" e1 e2
    X.Gt  e1 e2 -> binOp ">" e1 e2
    X.Lt  e1 e2 -> binOp "<" e1 e2
    X.Eq  e1 e2 -> binOp "==" e1 e2
    X.And e1 e2 -> binOp "&&" e1 e2
    X.Or  e1 e2 -> binOp "||" e1 e2
    X.Not e -> ("(!" ++) . (++ ")") <$> cXp e

    (X.IfThenElse cond eTrue eFalse :: Xp retType) -> do
        funName <- freshId
        Name resVar <- freshId
        eTrueStr <- cXp eTrue
        eFalseStr <- cXp eFalse
        let def = concat
                [ X.cType @retType, " ", unName funName,
                    "(", X.cType @Bool, " condition) {\n"
                , "    ", X.cType @retType, " ", resVar, ";\n"
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

    (X.Case (X.Scrut scrutName (scrut :: Xp scrutType)) matches :: Xp retType)
        -> do

        funName <- freshId

        newCaseFun @retType @scrutType funName (Name scrutName, matches)
        scrutStr <- cXp scrut

        pure $ mconcat [unName funName, "(", scrutStr, ")"]

    X.Case2
        (Proxy :: Proxy pt)
        (X.Scrut scrutName (transformee :: Xp t))
        (body :: Xp retType)
        -> do

        let sName = X.structName @pt

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
        Name retVar <- freshId
        bodyStr <- cXp body
        let retType = X.cType @retType
        let def = concat
                [ retType, " ", unName funName, "(", sName, "* ", argName, ") {\n"
                , "    ", scrutName, " = ", argName, ";\n"
                , "    ", retType, " ", retVar, " = ", bodyStr, ";\n"
                , "    return ", retVar, ";\n"
                , "}\n"
                ]
        Lens.Mtl.modifying cmDefs (def :)
        pure $ concat [unName funName, "(", getStructCall, ")"]

    X.SVar name -> pure name

    X.SFieldRef (X.FieldRef name idx :: X.FieldRef pt) -> do
        let (X.Field _ s _) = X.toFields (X.dummy @pt) !! idx
        pure $ concat [name, "->", s]

    X.Cast t e -> do
        eStr <- cXp e
        pure $ concat ["((", X.showTRep t, ") ", eStr, ")"]

  where
    binOp :: (Show a, Show b) => String -> Xp a -> Xp b -> Compile String
    binOp op e1 e2 = do
        s1 <- cXp e1
        s2 <- cXp e2
        pure $ mconcat ["(", s1, " ", op, " ", s2, ")"]

{- | Convert the body of a sum pattern match ('Xp.Case') to a new function
definition, and add it to the compilation state.
-}
newCaseFun :: forall retType scrutType .
    ( Show retType
    , X.CType retType    -- Return type of function.
    , X.CType scrutType  -- Type of scrutinee.
    )
    => Name
    -- ^ Name of function.
    -> (Name, [(Xp Bool, Xp retType)])
    -- ^ Name of scrutinee, and branches/matches of case-expression.
    -> Compile ()
newCaseFun (Name funName) (Name scrutName, matches) = do
    -- Get strings for the return type and scrutinee type.
    let retType = X.cType @retType
    let scrutType = X.cType @scrutType

    -- Generate a variable to contain the return value.
    retVar <- freshId

    -- Calculate the branches of the case.
    matchesStr <- mapM (cMatch retVar) matches

    -- Introduce a global variable to hold the value of the scrutinee. Needed
    -- so that nested case-expressions (leading to nested function calls)
    -- still keeps the variable in scope.
    newGlobalVar scrutType scrutName

    let def = mconcat
            [ retType, " ", funName, "(", scrutType, " arg) {\n"
            , "    ", scrutName, " = arg;\n"
            , "    ", retType, " ", unName retVar, ";\n"
            , concatMap ("    " ++) matchesStr

            -- Hacky thing to soak the final dangling "else" We ideally want to
            -- perform the exhaustiveness check in Haskell instead. But it looks
            -- a bit empty otherwise.
            , "    { printf(\"Non-exhaustive conditions in function `",
                    funName, "`\\n\"); }\n"

            , "    return ", unName retVar, ";\n"
            , "}\n"
            ]
    Lens.Mtl.modifying cmDefs (def :)
  where
    cMatch :: Name -> (Xp Bool, Xp retType) -> Compile String
    cMatch (Name retVar) (cond, body) = do
        condStr <- cXp cond
        bodyStr <- cXp body
        pure $ mconcat ["if (", condStr, ") { ", retVar, " = ", bodyStr, "; } else\n"]

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
newStructReturnerDef :: forall transformeeType pt .
    ( X.ToStruct transformeeType pt
    , X.CType transformeeType
    )
    => Name        -- ^ Name of function.
    -> Compile ()
newStructReturnerDef (Name funName) = do
    -- Get string for the scrutinee type
    let transformeeType = X.cType @transformeeType

    -- Introduce a global variable to hold the value of the transformee. Needed
    -- so that nested case-expressions (leading to nested function calls)
    -- still keeps the variable in scope.
    Name transformeeId <- freshId
    newGlobalVar transformeeType transformeeId

    -- v This shouldn't be done here, do it before saving it in the AST.
    let struct = X.toStruct @transformeeType @pt (X.SVar transformeeId)
    let sName = X.structName @pt

    -- Create the code for instantiating the struct.
    insts <- mapM instField (X.toFields @pt struct)

    Name retVar <- freshId
    let def = concat
            [ sName, "* ", funName, "(", transformeeType, " arg) {\n"
            , "    ", transformeeId, " = arg;\n"
            -- TODO: SET UP free() SOMEHOW SO WE DON'T GET MEMORY LEAKS HERE
            , "    ", sName, "* ", retVar, " = malloc(sizeof(", sName, "));\n"
            , concatMap (\ xs -> "    " ++ retVar ++ xs ++ ";\n") insts
            , "    return ", retVar, ";\n"
            , "}\n"
            ]
    Lens.Mtl.modifying cmDefs (def :)
  where
    instField :: X.Field -> Compile String
    instField (X.Field _ s v) = do
        vStr <- cXp v
        pure $ concat ["->", s, " = ", vStr]

-- | Create a struct definition and add it to the compilation state.
newStructDef :: forall pt . X.Struct pt => Compile ()
newStructDef = Lens.Mtl.modifying cmStructs (M.insert (Name nameStr) def)
  where
    nameStr :: String
    nameStr = X.structName @pt

    fieldsStr :: [String]
    fieldsStr = map showField $ X.toFields (X.dummy @pt)
      where
        showField :: X.Field -> String
        showField (X.Field t s _) = X.showTRep t ++ " " ++ s ++ ";\n"

    def :: String
    def = concat
            [ "typedef struct ", nameStr, " {\n"
            , concatMap ("    " ++) fieldsStr
            , "} ", nameStr, ";\n"
            ]
