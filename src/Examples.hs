{-# LANGUAGE TypeApplications #-}

module Examples where

import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8)

import HExp


testSign :: Float -> HExp Int
testSign x = hval x `match` inspect
  where
    inspect :: PatSign Float -> HExp Int
    inspect pf = hval $ case pf of
        Pos  -> 1
        Neg  -> -1
        Zero -> 0

testAscii :: String -> HExp Bool
testAscii s1 = hval s1 `match` inspect
  where
    inspect :: PatAscii String -> HExp Bool
    inspect s = hval $ case s of
        Ascii -> True
        Other -> False

testNot :: Bool -> HExp Bool
testNot b = hval b `match` inspect
  where
    -- Ugly use of Identity constructor is required currently
    inspect :: Identity Bool -> HExp Bool
    inspect (Identity b') = hval (not b')

testUnit :: String -> HExp Bool
testUnit s = match @() (HVar s) inspect
  where
    inspect :: Identity () -> HExp Bool
    inspect _ = hval True

-- Take some variable?
testSign2 :: HExp Int
testSign2 = match @Int8 (HVar "x") inspect
  where
    inspect :: PatSign Int8 -> HExp Int
    inspect pf = hval $ case pf of
        Pos  -> 1
        Neg  -> -1
        Zero -> 0