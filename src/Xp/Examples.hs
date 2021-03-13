{- | Examples using Xp.

Print the C output of a program @(p :: Xp a)@ with

> printProg p
-}

{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Xp.Examples where

import Data.Data
import GHC.Generics

import Xp.Core
import Xp.Compile (compile)


printProg :: Show a => Xp a -> IO ()
printProg = putStrLn . compile

{- | Increment by 1 for positive values, return 0 otherwise.

@
GHCi> printProg $ xprog1 (xvar "scrut")
{
int scrut = scrut;
int result;
if (scrut > 0) { result = scrut + 1 }
if (scrut < 0) { result = 0 }
}
@
-}
-- xprog1 :: Xp Int -> Xp Int
-- xprog1 var = xcase var conds bodies
--   where
--     conds :: Xp Int -> [Xp Bool]
--     conds sv = [sv >. 0, sv <. 0]

--     bodies :: Int -> Xp Int -> Xp Int
--     bodies = \case
--         0 -> pos
--         1 -> neg

--     pos :: Xp Int -> Xp Int
--     pos = (+ 1)

--     neg :: Xp Int -> Xp Int
--     neg = const 0

{- | Return a string depending on the value of the scrutinee.

@
GHCi> printProg xprog2
{
int scrut = scrutinee - 12;
int result;
if (scrut > 0) { result = "The number is Positive!" }
if (scrut < 0) { result = "The number is Negative!" }
if (scrut == 0) { result = "The number is Zero!" }
}
@
-}
-- xprog2 :: Xp String
-- xprog2 = xcase scrut conds bodies
--   where
--     scrut :: Xp Int
--     scrut = xvar "scrutinee" - 12

--     conds :: Xp Int -> [Xp Bool]
--     conds sv = [sv >. 0, sv <. 0, sv ==. 0]

--     bodies :: Int -> Xp Int -> Xp String
--     bodies = \case
--         0 -> const $ xval "The number is Positive!"
--         1 -> const $ xval "The number is Negative!"
--         2 -> const $ xval "The number is Zero!"

-- -- | Simple nested case expressions.
-- xprog3 :: Xp Int
-- xprog3 = 3 + xprog1 (xprog1 2)

-- case2 :: Xp Int -> (Partition Int -> Xp a) -> Xp a
-- data Partition Int = Pos (Expr Int) | Neg (Expr Int)   <- Type families?

-- case2 scrutiny $ \case
--  Pos exp -> ...
--  Neg exp -> ...`

-- (\case ... ) (Pos (Var Sym))
-- (\case ... ) (Neg (Var Sym))
-- case2 (var :: Xp) $ \case
--  Pos exp -> exp .+ 1
--  Neg exp -> 0

-- case :: Xp e -> (Partition e -> Xp a) -> Xp a

-- case x f
-- f (Pos (SymVar))
-- f (Neg (SymVar))
-- Apply to all cases, get a list of expressions:
-- [ Sym Var .+ 1, Const 0]

data Num a => Sig a
    = Pos
    | Neg
    deriving (Show, Enum, Bounded)

instance Partition Sig Int where
    conds var = [var >. 0, var <. 0]

ex1 :: Xp Int -> Xp Int
ex1 var = case' var $ \case
    Pos -> SVar + 1
    Neg -> 0
