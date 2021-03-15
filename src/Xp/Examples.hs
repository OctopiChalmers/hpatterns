{- | Examples using Xp.

Print the C output of a program @(p :: Xp a)@ with

> printProg p
-}

{- Caused by having to define Partition data types elsewhere, due to
limitations of TH.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Xp.Examples where

import Xp.Core
import Xp.Compile (compile)
import Xp.ExamplesTypes
import Xp.TH


printProg :: Show a => Xp a -> IO ()
printProg = putStrLn . compile

-- * Example 1

instance (Num a, Show a, Eq a) => Partition Sig a where
    conds var = [var >. 0, var <. 0, var ==. 0]

    {- | 'makeConstructors' generates appropriate values for the input type,
    since we always want any (Xp a) occurrences to be the SVar constructor.
    See documentation for 'makeConstructors' for examples.
    -}
    constructors = $(makeConstructors ''Sig)

ex1 :: Xp Int -> Xp Int
ex1 var = case' var $ \case
    Pos e -> e + 1
    Neg e -> e
    Zero  -> 0

-- * Example 2

instance Partition PartChar Char where
    conds var = [var ==. xval 'A', var /=. xval 'A']
    constructors = $(makeConstructors ''PartChar)

ex2 :: Xp Char -> Xp Int
ex2 var = case' var $ \case
    CharA _    -> 1
    CharNotA _ -> 0

-- * Example 3

{- | Nested case-expressions generate duplicate function calls!

@
GHCi> printProg $ needsWater 24 0.2

// Code generated from Xp program

int v0(int scrut) {
    int v1;
    if ((scrut < 0.2)) { v1 = True }
    if (((0.2 < scrut) && (scrut < 0.8))) { v1 = v2(24.0) }
    if ((0.8 < scrut)) { v1 = v4(24.0) }
    return v1;
}

int v4(int scrut) {
    int v5;
    if ((scrut < 18.0)) { v5 = False }
    if (((18.0 < scrut) && (scrut < 30.0))) { v5 = False }
    if ((30.0 < scrut)) { v5 = (scrut > 35.0) }
    return v5;
}

int v2(int scrut) {
    int v3;
    if ((scrut < 18.0)) { v3 = False }
    if (((18.0 < scrut) && (scrut < 30.0))) { v3 = False }
    if ((30.0 < scrut)) { v3 = (scrut > 35.0) }
    return v3;
}

int main() {
    int output = v0(0.2);
    printf("Program output is: %d", output)
    return 0;
}
@
-}
type MoisturePercentage = Double
type Temp               = Double
needsWater :: Xp Temp -> Xp MoisturePercentage -> Xp Bool
needsWater temp moistLvl =
    case' moistLvl $ \case
        MoistureThirsty _ -> xval True
        _ -> case' temp $ \case
            TempHot temp -> temp >. 35
            _ -> xval False

instance Partition PartMoisture Double where
    conds var =
        [ var <. 0.2
        , 0.2 <. var &&. var <. 0.8
        , 0.8 <. var
        ]
    constructors = $(makeConstructors ''PartMoisture)

instance Partition PartTemp Double where
    conds var =
        [ var <. 18
        , 18 <. var &&. var <. 30
        , 30 <. var
        ]
    constructors = $(makeConstructors ''PartTemp)
