{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module XpCompile where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as St
import qualified Data.List as List
import qualified Data.Map.Strict as M

import Lens.Micro.Platform

import Xp


--
-- * Compilation utils.
--

newtype Name = Name
    { unName :: String
    } deriving newtype (Show)
      deriving stock (Eq, Ord)

data Cm = Cm
    { _cmCounter :: Int
    , _cmFuns    :: [String]
    }
makeLenses ''Cm

-- | Return a unique identifier and increment the counter in state.
freshId :: Compile Name
freshId = do
    newId <- ('v' :) . show <$> use cmCounter
    modifying cmCounter (+ 1)
    pure $ Name newId

type Compile = R.ReaderT Name (St.State Cm)

--
-- * Main compilation stuff.
--

compile :: Show a => Xp a -> String
compile program =
    let (code, st) :: (String, Cm) =
            St.runState (R.runReaderT (cXp program) initName) initCm
        main = mainWrap code
    in mconcat
        [ "\n// Code generated from Xp program \n\n"
        , concatMap (++ "\n") (view cmFuns st)
        , main
        ]
  where
    initName :: Name
    initName = Name "scrut"

    initCm :: Cm
    initCm = Cm
        { _cmCounter = 0
        , _cmFuns = []
        }

    mainWrap :: String -> String
    mainWrap body = mconcat
        [ "int main() {\n"
        , "    int output = ", body, ";\n"
        , "    printf(\"Program output is: %d\", output)\n"
        , "    return 0;\n"
        , "}\n"
        ]

cXp :: Show a => Xp a -> Compile String
cXp = \case
    Val v -> pure $ show v
    Var s -> pure $ show s
    Add e1 e2 -> binOp "+" e1 e2
    Mul e1 e2 -> binOp "*" e1 e2
    Sub e1 e2 -> binOp "-" e1 e2
    Gt  e1 e2 -> binOp ">" e1 e2
    Lt  e1 e2 -> binOp "<" e1 e2
    Eq  e1 e2 -> binOp "==" e1 e2
    And e1 e2 -> binOp "&&" e1 e2
    Or  e1 e2 -> binOp "||" e1 e2
    Not e -> (\ eStr -> "(-" <> eStr <> ")") <$> cXp e

    Case scrut matches -> do
        funName <- freshId
        newCaseFun funName matches

        scrutStr <- cXp scrut
        pure $ mconcat [unName funName, "(", scrutStr, ")"]

    SVar -> unName <$> R.ask
  where
    binOp :: (Show a, Show b) => String -> Xp a -> Xp b -> Compile String
    binOp op e1 e2 = do
        s1 <- cXp e1
        s2 <- cXp e2
        pure $ mconcat ["(", s1, " ", op, " ", s2, ")"]

newCaseFun :: forall b . Show b => Name -> [(Xp Bool, Xp b)] -> Compile ()
newCaseFun (Name funName) matches = do
    resVar <- freshId
    matchesStr <- mapM (cMatch resVar) matches
    Name scrutName <- R.ask

    let def = mconcat
            [ "int ", funName, "(int ", scrutName, ") {\n"
            , "    int ", unName resVar, ";\n"
            , concatMap ("    " ++) matchesStr
            , "    return ", unName resVar, ";\n"
            , "}\n"
            ]
    modifying cmFuns (def :)
  where
    cMatch :: Name -> (Xp Bool, Xp b) -> Compile String
    cMatch (Name resVar) (cond, body) = do
        condStr <- cXp cond
        bodyStr <- cXp body
        pure $ mconcat ["if (", condStr, ") { ", resVar, " = ", bodyStr, " }\n"]
