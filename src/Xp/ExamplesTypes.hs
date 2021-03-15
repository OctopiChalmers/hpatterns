{- | This module serves solely to contain data types used for partitioning.

In other words, every type in this module exists for the purpose of creating
Partition instances in another module. This structure is entirely due to TH
limitations; we cannot create expressions with splicing on types defined in
the same module as the splice is used. This means that, because the splice is
intended to be used as the body of @Partition@ class method @concstructors@,
the data types for which the instance declaration is written for must be
__imported__.

This is pretty ugly as it is unintuitive and results in orphan instances.
Ideally, we'd like to automate and generalize the call to the
@makeConstructors@ function also, but that seems not possible with TH.

-}

module Xp.ExamplesTypes where

import Xp.Core

data Num a => Sig a
    = Pos (Xp a)
    | Neg (Xp a)
    | Zero
    deriving (Show)

data PartChar a
    = CharA (Xp Char)
    | CharNotA (Xp Char)
    deriving (Show)

data PartMoisture a
    = MoistureThirsty (Xp Double)
    | MoisturePerfect (Xp Double)
    | MoistureDrowning (Xp Double)
    deriving (Show)

data PartTemp a
    = TempCold (Xp Double)
    | TempOk (Xp Double)
    | TempHot (Xp Double)
    deriving (Show)
