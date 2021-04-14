{- | Generate code with 'printProg' from 'E.Compile'. -}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module E.Examples where

import Generics.SOP  -- for now

import E.Core

import qualified GHC.Generics as GG (Generic)


ex1 :: E Double -> Estate (E Int)
ex1 v = match @Sig v $ \case
    SOP    (Z (I n :* Nil)) -> n + 2
    SOP (S (Z Nil        )) -> 0
    SOP (S (S _          )) -> error "impossible by construction"

data Sig = Pos (E Int) | Neg
    deriving (GG.Generic, Generic)

instance Partition Sig Double where
    partition =
        [ \ v -> (v >. 0, Pos $ floorIntE v)
        , \ v -> (v <. 0, Neg)
        ]

ex2 :: E Double -> Estate (E Int)
ex2 v = matchM @Sig v $ \case
    SOP (S (S _          )) -> error "impossible by construction"
    SOP (S (Z Nil        )) -> pure 0
    SOP    (Z (I n :* Nil)) -> matchM @Size n $ \case
        SOP    (Z (I x :* I y :* Nil)) -> pure $ x + y
        SOP (S (Z (I x :* Nil)      )) -> pure $ x + n
        SOP (S (S _                 )) -> error "impossible by construction"

data Size = Large (E Int) (E Int) | Small (E Int)
    deriving (GG.Generic, Generic)

instance Partition Size Int where
    partition =
        [ \ v -> (v >. 100, Large v v)
        , \ v -> (valE True, Small (v + 1))
        ]
