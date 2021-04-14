{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

-- | Generate code with 'printProg' from 'E.Compile'.
module E.Examples where

import Generics.SOP  -- for now

import E.Core

import qualified GHC.Generics as GG (Generic)


data Sig = Pos (E Int) | Neg
    deriving (GG.Generic, Generic)

instance Partition Sig Double where
    partition =
        [ \ v -> (v >. 0, Pos $ floorIntE v)
        , \ v -> (v <. 0, Neg)
        ]

ex1 :: E Double -> Estate (E Int)
ex1 v = case' @Sig v $ \case
    SOP (Z (I n :* Nil)) -> n + 2
    SOP (S (Z Nil     )) -> 0
    SOP (S (S _       )) -> error "impossible by construction"

ex2 :: E Double -> E Double
ex2 v = fracPartE v * 20 + floorDoubleE v
