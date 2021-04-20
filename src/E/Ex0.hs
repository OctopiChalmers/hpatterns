{- | Example 0. Simplest variant, does not require SOP, but couples pattern
matching and partitioning tightly; we cannot simply pattern match on any
type, but only on those which we have defined some partitioning for.
-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module E.Ex0 where

import Generics.SOP  -- for now
import E.Core
import qualified GHC.Generics as GG (Generic)


ex0 :: E Double -> Estate (E Int)
ex0 v = match v $ \case
    T1 n          -> n + 1
    T2            -> 0
    T3 frac whole -> floorIntE (2 * frac * frac) + whole
    -- this causes redundancy in the C ^^^^   ^^^^

data T
    = T1 (E Int)
    | T2
    | T3 (E Double) (E Int)
    deriving (GG.Generic, Generic)
    -- Generic is not actually needed for this method, but the Partition
    -- class has a constraint on it.

instance Partition T Double where
    partition = undefined
        -- [ \ v -> (v >. 100, T3 (fracPartE v) (floorIntE v))
        -- , \ v -> (v >. 0,   T1 (floorIntE v))
        -- , \ v -> (v <. 0,   T2)
        -- ]



