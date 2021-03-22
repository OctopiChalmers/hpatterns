{- | Module containing things concering compilation from Xp to C code output.

Doofus implementation, basically just concatenating strings.
-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Xp.Compile where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as St
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens.Mtl
import qualified Lens.Micro.TH as Lens.TH

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
        , concatMap ((++ "\n") . includeWrap) ["stdio.h"], "\n"

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
        , _cmDefs = []
        , _cmGlobals = []
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
    Val v -> pure $ show v
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
                [ "int ", unName funName, "(condition) {\n"
                , "    int ", resVar, ";\n"
                , "    if (condition) {\n"
                , "        ", resVar, " = ", eTrueStr, ";\n"
                , "    else {\n"
                , "        ", resVar, " = ", eFalseStr, ";\n"
                , "    }\n"
                , "    return ", resVar, ";\n"
                , "}\n"
                ]
        Lens.Mtl.modifying cmDefs (def :)

        condStr <- cXp cond
        pure $ concat [unName funName, "(", condStr, ")"]

    Case (scrutName, scrut) matches -> do
        funName <- freshId

        newCaseFun funName (scrutName, matches)
        scrutStr <- cXp scrut

        pure $ mconcat [unName funName, "(", scrutStr, ")"]

    SVar (name, _idx) -> pure name
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
    newGlobalVar scrutName

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

newGlobalVar :: String -> Compile ()
newGlobalVar s =
    let varStr = concat ["int ", s, ";"]
    in Lens.Mtl.modifying cmGlobals (varStr :)
