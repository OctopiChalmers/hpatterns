{- | Examples using Xp.

Print the C output of a program @(p :: Xp a)@ with

> printProg p
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Xp.Examples where

import Xp.Core
import Xp.Compile (compile)
import Xp.TH


printProg :: Show a => Xp a -> IO ()
printProg = putStrLn . compile

instance (Num a, Show a, Eq a) => Partition Sig a where
    conds var = [var >. 0, var <. 0, var ==. 0]
    constructors = $(makeConstructors ''Sig)

ex1 :: Xp Int -> Xp Int
ex1 var = case' var $ \case
    Pos e -> e + 1
    Neg e -> e
    Zero  -> 0

instance Partition PartitionChar Char where
    conds var = [var ==. xval 'A', var /=. xval 'A']
    constructors = $(makeConstructors ''PartitionChar)

ex2 :: Xp Char -> Xp Int
ex2 var = case' var $ \case
    CharA _    -> 1
    CharNotA _ -> 0

type MoistureLvl = Double
type Temp        = Double
sufficientlyMoist :: Xp MoistureLvl -> Xp Double
sufficientlyMoist moistLvl = case' moistLvl $ \case
    Pos e -> e / 4
    Neg e -> 1 / e
    Zero  -> 0.0

