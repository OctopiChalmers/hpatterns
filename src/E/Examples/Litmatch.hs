{-# LANGUAGE LambdaCase #-}

{- | Module showing off literal pattern matching, using the Partition instances
provided by 'enumIdPartitions'.
-}

module E.Examples.Litmatch where

import Data.Int (Int8)

import E.Lang


litmatch :: E Int8 -> Estate (E Int8)
litmatch n = match n $ valE . \case
    0 -> 0
    1 -> 1
    x | x > 0     -> -x
      | otherwise -> x

