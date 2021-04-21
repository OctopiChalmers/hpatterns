{- | Example 1. Playing around with SOP. Very unergonomic; clearly
the user should not need to know anything about SOP.
-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module E.Ex1 where

import Generics.SOP
import E.Core
import qualified GHC.Generics as GG (Generic)


-- ex1 :: E Double -> Estate (E Int)
-- ex1 v = match @Sig v $ \case
--     SOP    (Z (I n :* Nil)) -> n + 2
--     SOP (S (Z Nil        )) -> 0
--     SOP (S (S _          )) -> error "impossible by construction"

data Sig = Pos (E Int) | Neg
    deriving (GG.Generic, Generic)

-- instance Partition Sig Double where
--     partition =
--         [ \ v -> (v >. 0, Pos $ floorIntE v)
--         , \ v -> (v <. 0, Neg)
--         ]
