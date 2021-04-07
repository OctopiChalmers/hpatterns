{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Xp.HalftimeExamples where

import Xp.Core
import Xp.Compile (compile)
import Xp.TH


data Signature = Pos | Neg | Zero deriving Enum
instance Partition Int Signature where
    partition n = [(Pos, n >. 0), (Neg, n <. 0), (Zero, n ==. 0)]

foo :: Xp Int -> Hiska (Xp Int)
foo var = branch var $ \case
    Pos  -> pure . (+ 1)
    Neg  -> pure . (* 3)
    Zero -> pure . const 99

data Size = Large | Small deriving Enum
instance Partition Int Size where
    partition var = [(Large, var >. 999), (Small, var <. 999)]

bar :: Xp Int -> Hiska (Xp Int)
bar var = branch var $ \case
    Zero -> const (pure 99)
    Neg  -> \ x -> pure (x - 1)
    Pos  -> \ x -> branch x $ \case
        Large -> \ _ -> pure (x - 100)
        Small -> \ y -> pure (y - 1)

data Vec = Vec (Xp Int) (Xp Int)
$(deriveStruct ''Vec)

-- Dummy implementation of toStruct, simply clones the input.
instance ToStruct Int Vec where
    toStruct n = Vec n n

baz :: Xp Int -> Hiska (Xp Int)
baz var = deconstruct var $ \ (Vec x y) -> x + y

data T
    = T1
    | T2
    deriving Enum

instance Partition Int T where
    -- Will crash at runtime
    partition = error "Definition intentionally omitted"

data Pair = Pair (Xp Int) (Xp Int)
$(deriveStruct ''Pair)

instance ToStruct Int Pair where
    -- Will crash at runtime
    toStruct = error "Definition intentionally omitted"

f :: Xp Int -> Hiska (Xp Int)
f x = branch x $ \case
    T1 ->      \ n          -> pure (n + 1)
    T2 -> as $ \ (Pair a b) -> (a + b)
