{- | Example 0. Simplest variant, does not require SOP, but couples pattern
matching and partitioning tightly; we cannot simply pattern match on any
type, but only on those which we have defined some partitioning for.
-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module E.Ex0 where

import E.Core


ex :: E Int -> Estate (E Int)
ex v = match v $ \case
    T2   -> pure 98
    T1 x -> match (x + 1000) $ \case
        T2   -> pure 99
        T1 _ -> pure (x + 2)

data T = T1 (E Int) | T2
instance Partition T Int where
    partition = [\ v -> (v >=. 0, T1 v), \ v -> (v <. 0, T2)]
