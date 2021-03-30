{- | Examples using Xp.

Print the C output of a program @(p :: Hiska (Xp a))@ with

> printProg p
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Xp.Examples where

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
-}
ex1 :: Xp Int -> Hiska (Xp Int)
ex1 var = branch var $ \case
    Pos  -> pure . (+ 1)
    Neg  -> pure
    Zero -> pure

data Sig = Pos | Neg | Zero
    deriving (Show, Enum)

instance (CType a, Eq a, Num a, Show a) => Partition a Sig where
    partition var =
        [ (Pos, var >. 0)
        , (Neg, var <. 0)
        , (Zero, var ==. 0)
        ]

--
-- * Example 2
--

{- | Return True if the input char is exactly 'A', False otherwise.
-}

ex2 :: Xp Char -> Hiska (Xp Bool)
ex2 var = branch var $ \case
    CharA    -> const $ pure $ xval True
    CharNotA -> const $ pure $ xval False

data PartChar
    = CharA
    | CharNotA
    deriving (Show, Enum)

instance Partition Char PartChar where
    partition var = [(CharA, var ==. xval 'A'), (CharNotA, var /=. xval 'A')]

--
-- * Example 3
--

{- | More complicated example with nested case-expressions.

In this fictional example, a plant needs watering if one of the following
conditions apply:

1) The soil is sufficiently dry (< 20 %)
2) The soil is only halfway moist (< 50 %) AND the temperature is
    very high (> 35 degrees).
-}
needsWatering :: Xp Temp -> Xp Moisture -> Hiska (Xp Bool)
needsWatering temp moistLvl = branch moistLvl $ \case
    MoistureOk -> \ m -> branch temp $ \case
        TempHot -> \ t -> pure $ t >. 35 &&. m <. 0.5
        _       -> const $ pure $ xval False
    MoistureDry -> const $ pure $ xval True

type Moisture = Double
type Temp     = Int

data PartMoisture
    = MoistureDry
    | MoistureOk
    deriving (Show, Enum)

data PartTemp
    = TempCold
    | TempOk
    | TempHot
    deriving (Show, Enum)

instance Partition Double PartMoisture where
    partition var =
        [ (MoistureDry, var <. 0.2)
        , (MoistureOk, var >. 0.2)
        ]

instance Partition Int PartTemp where
    partition var =
        [ (TempCold, var <. 18)
        , (TempOk, var >. 18 &&. var <. 30)
        , (TempHot, var >. 30)
        ]

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

ex4 :: Xp Double -> Hiska (Xp Double)
ex4 input = branch input $ \case
    Pos  -> as $ \ (SplitFrac int frac) -> frac
    Neg  -> pure
    Zero -> pure

--
-- * Example 5
--

data Size = Large | Small
    deriving (Show, Enum)

data Mirror = Mirror (Xp Float) (Xp Int)
    deriving Show
$(deriveStruct ''Mirror)

instance Partition Int Size where
    partition var = [(Large, var >. 9), (Small, var <. 9)]

instance ToStruct Int Mirror where
    toStruct n = Mirror n (-n)

prog5 :: Xp Int -> Hiska (Xp Int)
prog5 var = branch var $ \case
    Large -> as $ \ (Mirror x y) -> x + y
    Small -> pure . (+ 1)
