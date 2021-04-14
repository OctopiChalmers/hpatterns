{- | Generate code with 'printProg' from 'E.Compile'. -}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module E.Examples where

import E.Core

import Generics.SOP  -- for now


ex1 :: E Double -> Estate (E Int)
ex1 v = case' @Sig v $ \case
    SOP (Z (I n :* Nil)) -> n + 1
    SOP (S (Z Nil)) -> 0

    SOP (S (S _)) -> error "impossible by construction"

ex2 :: E Double -> E Double
ex2 v = (c_fracPart v) * 20 + c_floorDouble v

