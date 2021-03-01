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

-- combinedTest1 :: HExp Int8
combinedTest1 = case1 @C @_ @PatSign (HVar "x") pos neg
  where
    -- pos :: Num a => [HExp a] -> HExp a
    pos = (+ 1)

    -- neg :: Num a => [HExp a] -> HExp a
    neg = id

-- -- why are both of these x??
-- case x of
--     Pos x -> ???
--     Neg x -> ???

