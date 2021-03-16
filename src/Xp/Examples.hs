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

--
-- * Example 1
--

{- | Increment input by 1 if positive, otherwise return the input unchanged.

C output:

@
GHCi> printProg $ ex1 3

// Code generated from Xp program

int v0(int scrut) {
    int v1;
    if ((scrut > 0)) { v1 = (scrut + 1) }
    if ((scrut < 0)) { v1 = scrut }
    if ((scrut == 0)) { v1 = 0 }
    return v1;
}

int main() {
    int output = v0(3);
    printf("Program output is: %d", output)
    return 0;
}
@
-}
ex1 :: Xp Int -> Xp Int
ex1 var = case' var $ \case
    Pos e -> e + 1
    Neg e -> e
    Zero  -> 0

instance (Num a, Show a, Eq a) => Partition Sig a where
    conds var = [var >. 0, var <. 0, var ==. 0]

    {- | 'makeConstructors' generates appropriate values for the input type,
    since we always want any (Xp a) occurrences to be the SVar constructor.
    See documentation for 'makeConstructors' for examples.

    Here,

    > constructors = $(makeConstructors ''Sig)

    is equivalent to

    > constructors = [Pos SVar, Neg SVar]
    -}
    constructors = $(makeConstructors ''Sig)

--
-- * Example 2
--

{- | Return True if the input char is exactly 'A', False otherwise.

C output (probably doesn't actually run properly due to improper
string representation in C):

@
GHCi> printProg $ ex2 (xval 'A')

// Code generated from Xp program

int v0(int scrut) {
    int v1;
    if ((scrut == 'A')) { v1 = 1 }
    if ((!(scrut == 'A'))) { v1 = 0 }
    return v1;
}

int main() {
    int output = v0('A');
    printf("Program output is: %d", output)
    return 0;
}
@
-}
ex2 :: Xp Char -> Xp Bool
ex2 var = case' var $ \case
    CharA _    -> xval True
    CharNotA _ -> xval False

instance Partition PartChar Char where
    conds var = [var ==. xval 'A', var /=. xval 'A']
    constructors = $(makeConstructors ''PartChar)

--
-- * Example 3
--

{- | More complicated example with nested case-expressions.

CURRENTLY DOES NOT BEHAVE CORRECTLY, SEE C OUTPUT BELOW.

In this fictional example, a plant needs watering if one of the following
conditions apply:

1) The soil is sufficiently dry (< 20 %)
2) The soil is only halfway moist (< 50 %) AND the temperature is
    very high (> 35 degrees).

C Output:

Note the incorrect behavior caused by nested case expressions; while in
Haskell we have values from both patterns in scope, the compilation does
not. NEEDS FIXING.

@
GHCi> printProg $ needsWatering 25 0.3

// Code generated from Xp program

int v0(int scrut) {
    int v1;
    if ((scrut < 0.2)) { v1 = True }
    if ((0.2 < scrut)) { v1 = v2(25) }
    return v1;
}

int v2(int scrut) {
    int v3;
    if ((scrut < 18)) { v3 = False }
    if (((18 < scrut) && (scrut < 30))) { v3 = False }
    if ((30 < scrut)) { v3 = ((scrut > 35) && (scrut < 0.5)) }
    return v3;
}

int main() {
    int output = v0(0.3);
    printf("Program output is: %d", output)
    return 0;
}
@
-}
needsWatering :: Xp Int -> Xp Moisture -> Xp Bool
needsWatering temp moistLvl = case' moistLvl $ \case
    MoistureDry _       -> xval True
    MoistureOk moistLvl -> case' temp $ \case
        TempHot temp -> temp >. 35 &&. moistLvl <. 0.5
        _            -> xval False

type Moisture = Double
type Temp     = Int

instance Partition PartMoisture Double where
    conds var =
        [ var <. 0.2  -- Dry
        , 0.2 <. var  -- OK
        ]
    constructors = $(makeConstructors ''PartMoisture)

instance Partition PartTemp Int where
    conds var =
        [ var <. 18                -- Cold
        , 18 <. var &&. var <. 30  -- OK
        , 30 <. var                -- Hot
        ]
    constructors = $(makeConstructors ''PartTemp)
