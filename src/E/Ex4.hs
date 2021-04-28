{- | Example 4. Showing the simpler version like Example 1, but with
a nested case.
-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module E.Ex4 where

import E.Core

ex :: E Int -> E Int
ex v = match v $ \case
    T2   -> 98
    T1 x -> match (x + 1) $ \case
        T2   -> 99
        T1 y -> y + 2

data T = T1 (E Int) | T2
instance Partition T Int where
    partition = [\ v -> (v >=. 0, T1 v), \ v -> (v <. 0, T2)]
