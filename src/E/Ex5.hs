{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module E.Ex5 where

import E.Core


data A
    = A1 (E Int)
    | A2 (E Bool) (E Int)

instance Partition A Int where
    partition =
        [ \ v -> (v >. 0, A1 (ETag "_tag_A1_0" $ v + 5))
        , \ v -> (v <. 0, A2 (ETag "_tag_A2_0" $ v <. -3)
                             (ETag "_tag_A2_1" $ v - 5))
        ]

ex5 :: E Int -> Estate (E Bool)
ex5 x = matchM x $ \case
    A2 b n -> pure $ n ==. n ||. b
    A1 n   -> match n $ \case
        A1 s    -> n >. 1
        A2 t _y -> t

