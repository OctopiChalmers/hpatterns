{- | Example 2. Same as Example 1 but with a nested case. -}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module E.Ex2 where

import Generics.SOP
import E.Core
import qualified GHC.Generics as GG (Generic)


ex2 :: E Double -> Estate (E Int)
ex2 v = matchM @Sig v $ \case
    SOP (S (S _          )) -> error "impossible by construction"
    SOP (S (Z Nil        )) -> pure 0
    SOP    (Z (I n :* Nil)) -> matchM @Size n $ \case
        SOP    (Z (I x :* I y :* Nil)) -> pure $ x + y
        SOP (S (Z (I x :* Nil)      )) -> pure $ x + n
        SOP (S (S _                 )) -> error "impossible by construction"

data Size
    = Large (E Int) (E Int)
    | Small (E Int)
    deriving (Generic, GG.Generic)

instance Partition Size Int where
    partition =
        [ \ v -> (v >. 100, Large v v)
        , \ v -> (valE True, Small (v + 1))
        ]

data Sig = Pos (E Int) | Neg
    deriving (GG.Generic, Generic)

instance Partition Sig Double where
    partition =
        [ \ v -> (v >. 0, Pos $ floorIntE v)
        , \ v -> (v <. 0, Neg)
        ]
