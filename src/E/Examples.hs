{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate code with 'printProg' from 'E.Compile'.
module E.Examples where

import E.Core
import Generics.SOP  -- for now


ex1 :: E Double -> Estate (E Int)
ex1 v = case' @Sig v $ \case
    SOP (Z (I n :* Nil)) -> n + 2
    SOP (S (Z Nil     )) -> 0
    -- SOP (S (S _       )) -> error "impossible by construction"

ex2 :: E Double -> E Double
ex2 v = fracPartE v * 20 + floorDoubleE v
