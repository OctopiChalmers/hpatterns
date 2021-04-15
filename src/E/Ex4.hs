{- | Example 4. Showing the simpler version like Example 1, but with
a nested case.
-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module E.Ex4 where

import Generics.SOP
import E.Core
import qualified GHC.Generics as GG (Generic)


ex4 :: E Double -> Estate (E Int)
ex4 v = matchM' v $ \case
    Neg   -> pure 0
    Pos n -> matchM' n $ \case
        Large x y -> pure (x + y)
        Small x   -> pure (x + n)

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
