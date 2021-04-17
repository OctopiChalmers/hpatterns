{- | Example 3. Using pattern synonyms, we can improve ergonomics
from Example 2. Still, these should be automatically generated.
-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeApplications      #-}

module E.Ex3 where

import Generics.SOP
import E.Core
    ( valE, matchM, (<.), floorIntE, (>.), Partition(..), Estate, E )
import qualified GHC.Generics as GG (Generic)


-- The COMPLETE pragmas are needed to trick GHC to believe that we, in fact,
-- will will not have non-exhaustive patterns.

pattern Pos_ :: E Int -> Rep Sig
pattern Pos_ n = SOP (Z (I n :* Nil))
pattern Neg_ :: Rep Sig
pattern Neg_ = SOP (S (Z Nil))
{-# COMPLETE Neg_, Pos_ #-}

pattern Large_ :: E Int -> E Int -> Rep Size
pattern Large_ x y = SOP (Z (I x :* I y :* Nil))
pattern Small_ :: E Int -> Rep Size
pattern Small_ x = SOP (S (Z (I x :* Nil)))
{-# COMPLETE Large_, Small_ #-}


-- ex3 :: E Double -> Estate (E Int)
-- ex3 v = matchM @Sig v $ \case
--     Neg_   -> pure 0
--     Pos_ n -> matchM @Size n $ \case
--         Large_ x y -> pure (x + y)
--         Small_ x   -> pure (x + n)

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
