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


printProg :: Show a => Hiska (Xp a) -> IO ()
printProg = putStrLn . compile . runHiska

--
-- * Example 1
--

{- | Increment input by 1 if positive, otherwise return the input unchanged.

C output:

@
GHCi> printProg $ ex1 4

int coreId0;

int v0(int v2) {
    coreId0 = v2;
    int v1;
    if ((coreId0 > 0)) { v1 = (coreId0 + 1); } else
    if ((coreId0 < 0)) { v1 = coreId0; } else
    if ((coreId0 == 0)) { v1 = 0; } else
    { printf("Non-exhaustive conditions in function `v0`\n"); }
    return v1;
}

int main() {
    int output = v0(4);
    printf("Program output is: %d\n", output);
    return 0;
}
@
-}
ex1 :: Xp Int -> Hiska (Xp Int)
ex1 var = case' var $ \case
    Pos e -> pure (e + 1)
    Neg e -> pure e
    Zero  -> pure 0

instance (Num a, Show a, Eq a) => Partition Sig a where
    partition var = PartitionData preds constructors
      where
        preds = [var >. 0, var <. 0, var ==. 0]
        constructors = [Pos var, Neg var, Zero]

--
-- * Example 2
--

{- | Return True if the input char is exactly 'A', False otherwise.

C output (doesn't actually run due to improper string representation in C):

@
GHCi> printProg $ ex2 (xval 'A')

int coreId0;

int v0(int v2) {
    coreId0 = v2;
    int v1;
    if ((coreId0 == 'A')) { v1 = True; } else
    if ((!(coreId0 == 'A'))) { v1 = False; } else
    { printf("Non-exhaustive conditions in function `v0`\n"); }
    return v1;
}

int main() {
    int output = v0('A');
    printf("Program output is: %d\n", output);
    return 0;
}
@
-}
ex2 :: Xp Char -> Hiska (Xp Bool)
ex2 var = case' var $ \case
    CharA _    -> pure $ xval True
    CharNotA _ -> pure $ xval False

instance Partition PartChar Char where
    partition var = PartitionData preds constructors
      where
        preds = [var ==. xval 'A', var /=. xval 'A']
        constructors = [CharA var, CharNotA var]

--
-- * Example 3
--

{- | More complicated example with nested case-expressions.

In this fictional example, a plant needs watering if one of the following
conditions apply:

1) The soil is sufficiently dry (< 20 %)
2) The soil is only halfway moist (< 50 %) AND the temperature is
    very high (> 35 degrees).

C Output:

@
GHCi> printProg $ needsWatering 25 0.3

int coreId0;
int coreId1;

int v2(int v4) {
    coreId1 = v4;
    int v3;
    if ((coreId1 < 18)) { v3 = False; } else
    if (((18 < coreId1) && (coreId1 < 30))) { v3 = False; } else
    if ((30 < coreId1)) { v3 = ((coreId1 > 35) && (coreId0 < 0.5)); } else
    { printf("Non-exhaustive conditions in function `v2`\n"); }
    return v3;
}

int v0(int v5) {
    coreId0 = v5;
    int v1;
    if ((coreId0 < 0.2)) { v1 = True; } else
    if ((0.2 < coreId0)) { v1 = v2(25); } else
    { printf("Non-exhaustive conditions in function `v0`\n"); }
    return v1;
}

int main() {
    int output = v0(0.3);
    printf("Program output is: %d\n", output);
    return 0;
}
@
-}
needsWatering :: Xp Temp -> Xp Moisture -> Hiska (Xp Bool)
needsWatering temp moistLvl = case' moistLvl $ \case
    MoistureOk m -> case' temp $ \case
        TempHot t -> pure $ t >. 35 &&. m <. 0.5
        _         -> pure $ xval False
    MoistureDry _ -> pure $ xval True

type Moisture = Double
type Temp     = Int

instance Partition PartMoisture Double where
    partition var = PartitionData preds constructors
      where
        preds =
            [ var <. 0.2  -- Dry
            , 0.2 <. var  -- OK
            ]
        constructors =
            [ MoistureDry var
            , MoistureOk  var
            ]

instance Partition PartTemp Int where
    partition var = PartitionData preds constructors
      where
        preds =
            [ var <. 18                -- Cold
            , 18 <. var &&. var <. 30  -- OK
            , 30 <. var                -- Hot
            ]
        constructors =
            [ TempCold var
            , TempOk   var
            , TempHot  var
            ]
