module XpCompile where

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as St
import qualified Data.Map.Strict as M

import Xp


compileXp :: Show a => Xp a -> String
compileXp e = R.runReader (prettyXp e) initEnv
  where
    initEnv :: Env
    initEnv = Env Nothing Nothing

data Env = Env
    { envScrut :: Maybe String
    , envRes :: Maybe String
    }

prettyXp :: forall a .
    ( Show a
    )
    => Xp a
    -> R.Reader Env String
prettyXp = \case
    Var s -> pure s
    Val v -> pure $ show v
    Add e1 e2 -> binOp "+" e1 e2
    Mul e1 e2 -> binOp "*" e1 e2
    Sub e1 e2 -> binOp "-" e1 e2
    Gt e1 e2  -> binOp ">" e1 e2
    Lt e1 e2  -> binOp "<" e1 e2
    Eq e1 e2  -> binOp "==" e1 e2
    And e1 e2 -> binOp "&&" e1 e2
    Or e1 e2  -> binOp "||" e1 e2
    Not e    -> ('!' :) <$> prettyXp e
    SymVar -> R.asks envScrut >>= \case
        Nothing -> error "no scrutinee variable in environment"
        Just s  -> pure s
    Case scrut matches -> do
        let newVar = "scrut"  -- hardcoded
        let resVar = "result"
        let newEnv = Env
                { envScrut = Just newVar
                , envRes   = Just resVar
                }
        scrutStr <- prettyXp scrut
        matchesStrs <- R.local (const newEnv) (mapM prettyXpMatch matches)
        pure $ "{\n"
            <> "int " <> newVar <> " = " <> scrutStr <> ";\n"
            <> "int " <> resVar <> ";\n"
            <> unlines matchesStrs
            <> "}\n"
  where
    binOp :: (Show x, Show y) => String -> Xp x -> Xp y -> R.Reader Env String
    binOp opStr e1 e2 = do
        e1Str <- prettyXp e1
        e2Str <- prettyXp e2
        pure $ unwords [e1Str, opStr, e2Str]

    prettyXpMatch :: (Xp Bool, Xp a) -> R.Reader Env String
    prettyXpMatch (cond, body) = do
        condStr <- prettyXp cond
        bodyStr <- prettyXp body
        resVar <- R.asks envRes >>= \case
            Nothing -> error "no return variable in environment"
            Just s -> pure s
        pure $ "if (" <> condStr <> ") { " <> resVar <> " = " <> bodyStr <> " }"
