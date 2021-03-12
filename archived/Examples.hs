module Examples where

import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8)

import HExp


testSign :: Float -> HExp Int
testSign x = HVal x `hmatchPart` inspect
  where
    inspect :: PatSign Float -> HExp Int
    inspect pf = HVal $ case pf of
        Pos  -> 1
        Neg  -> -1
