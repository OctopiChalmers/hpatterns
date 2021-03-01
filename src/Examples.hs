{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Examples where

import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8)

import HExp

-- Test programs for partition patterns.

testSign :: Float -> HExp Int
testSign x = hval x `hmatchPart` inspect
  where
    inspect :: PatSign Float -> HExp Int
    inspect pf = hval $ case pf of
        Pos  -> 1
        Neg  -> -1
        Zero -> 0

testAscii :: String -> HExp Bool
testAscii s1 = hval s1 `hmatchPart` inspect
  where
    inspect :: PatAscii String -> HExp Bool
    inspect s = hval $ case s of
        Ascii -> True
        Other -> False

testNot :: Bool -> HExp Bool
testNot b = hval b `hmatchPart` inspect
  where
    -- Ugly use of Identity constructor is required currently
    inspect :: Identity Bool -> HExp Bool
    inspect (Identity b') = hval (not b')

testUnit :: String -> HExp Bool
testUnit s = hmatchPart (HVar s) inspect
  where
    inspect :: Identity Bool -> HExp Bool
    inspect _ = hval True

-- Take some variable?
testSign2 :: HExp Int
testSign2 = hmatchPart @Int8 (HVar "x") inspect
  where
    inspect :: PatSign Int8 -> HExp Int
    inspect pf = hval $ case pf of
        Pos  -> 1
        Neg  -> -1
        Zero -> 0

-- Test programs for product type patterns

ptTest1 :: HExp B
ptTest1 = case1 (B True) inspect
  where
    inspect = hnot

-- ptTest2 :: HExp Int8 -> HExp Int8
-- ptTest2 e = case1 e inspect
--   where
--     inspect HPVar = HPVar + hval 1
