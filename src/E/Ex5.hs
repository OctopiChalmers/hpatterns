{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns          #-}

module E.Ex5 where

import Data.Functor ((<&>))

import E.Core


data A
    = A1 (E Int)
    | A2 (E Bool) (E Int)

instance Partition A Int where
    partition =
        [ \ v -> (v >. 0, newFieldTag <&> \ t -> A1 (t $ v + 5))
        , \ v -> (v <. 0, do
            t1 <- newFieldTag
            t2 <- newFieldTag
            pure $ A2 (t1 $ v <. -3) (t2 $ v - 5))
        ]


ex5 :: E Int -> Estate (E Bool)
ex5 x = matchM x $ \case
    A2 b n -> pure $ n ==. n ||. b
    A1 n   -> match n $ \case
        A1 s    -> n >. 1
        A2 t _y -> t

