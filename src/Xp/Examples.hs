{- | Examples using Xp.

Print the C output of a program @(p :: Hiska (Xp a))@ with

> printProg p
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Xp.Examples where

import Data.Bifunctor (first)

import Xp.Core
import Xp.Compile (compile)
import Xp.TH


writeProg :: (CType a, Show a)
    => FilePath
    -> Hiska (Xp a)
    -> IO ()
writeProg fp = writeFile fp . compile . runHiska

printProg :: (CType a, Show a)
    => Hiska (Xp a)
    -> IO ()
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

data Num a => Sig a
    = Pos (Xp a)
    | Neg (Xp a)
    | Zero
    deriving (Show)

instance (CType a, Num a, Show a, Eq a) => Partition Sig a where
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

data PartChar a
    = CharA (Xp Char)
    | CharNotA (Xp Char)
    deriving (Show)

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

data PartMoisture a
    = MoistureDry (Xp Double)
    | MoistureOk  (Xp Double)
    deriving (Show)

data PartTemp a
    = TempCold (Xp Int)
    | TempOk (Xp Int)
    | TempHot (Xp Int)
    deriving (Show)

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

-- | Re-implementation of watering example, but with if/then/else.
water2 :: Xp Temp -> Xp Moisture -> Hiska (Xp Bool)
water2 temp moistLvl = pure $
    ifte moistureOk
        (ifte tempHot
            (temp >. 35 &&. moistLvl <. 0.5)
            (xval False))
        (xval True)
  where
    tempHot = temp >. 25
    moistureOk = moistLvl <. 0.2

--
-- * Example 4
--

data SplitFrac = SplitFrac
    { sfInt    :: (Xp Int)
    , sfDouble :: (Xp Double)
    } deriving Show

instance Struct SplitFrac where
    structName = "SplitFrac"
    toFields (SplitFrac int frac) =
        [ Field TInt "sfInt" int
        , Field TDouble "sfDouble" frac
        ]
    fromFields
        [ Field TInt "sfInt" int
        , Field TDouble "sfDouble" frac
        ]
        = SplitFrac int frac

    dummy = SplitFrac (error "dummy") (error "dummy")

instance ToStruct Double SplitFrac where
    -- | Split a double into its integer and fractional part
    toStruct double = SplitFrac int frac
      where
        int :: Xp Int
        int = cast TInt double

        frac :: Xp Double
        frac = double - (cast TDouble int)

{- | Return true if the fractional part of the input would round upwards
to the nearest whole number.
-}
ex4 :: Xp Double -> Hiska (Xp Double)
ex4 input = case2 input $ \case
    SplitFrac int frac -> frac

--
-- * Example 5
--

data Size
    = Large
    | Small
    deriving Show

class Part p a where
    branch :: Xp a -> [(p, Xp Bool)]

instance Part Size Int where
    branch var = [(Large, var >. 10), (Small, var <. 10)]

data Clone = Clone (Xp Int) (Xp Int)

instance Struct Clone where
    structName = "Clone"
    toFields (Clone x y) = [Field TInt "x" x, Field TInt "y" y]
    fromFields [Field TInt "x" x, Field TInt "y" y] = (Clone x y)
    dummy = Clone (error "dummy") (error "dummy")

instance ToStruct Int Clone where
    toStruct var = Clone var var

caseof :: forall a part struct b .
    ( ToStruct a struct
    , Part part a
    , Struct struct
    , Show a
    )
    => Xp a
    -> (part -> Maybe (Xp b))
    -> (struct -> Xp b)
    -> Hiska (Xp b)
caseof scrut fromPart fromStruct = do
    scrutId <- freshId

    let xs = branch @part @a (SVar scrutId)
        xs :: [(part, Xp Bool)]

    let result = map (first (f scrutId)) xs
        result :: [(Xp b, Xp Bool)]

    pure $ CaseOf (Scrut scrutId scrut) result
  where
    f :: String -> part -> Xp b
    f varName constr = case fromPart constr of
        Just res -> res
        Nothing  -> fromStruct (toStruct @a $ SVar varName)

symStruct :: forall struct .
    ( Struct struct
    )
    => String
    -> struct
symStruct scrutName =
    fromFields
    $ zipWith mkSymField [0 ..]
    $ toFields (dummy @struct)
  where
    mkSymField :: Int -> Field -> Field
    mkSymField idx (Field t s _) =
        Field t s $ SFieldRef @struct $ FieldRef scrutName idx

ex5 :: Xp Int -> Hiska (Xp Int)
ex5 var = caseof var f1 f2
  where
    f1 = \case
        Large -> Nothing
        Small -> Just var

    f2 = \case
        Clone x y -> x + y
