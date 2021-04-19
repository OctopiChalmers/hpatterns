{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module E.Ex5 where

import E.Core
import E.TH


data A
    = A1 (E Int)
    | A2 (E Bool) (E Int)
$(mkConstructors ''A)

instance Partition A Int where
    partition =
        [ \ v -> (v >. 0, A1_ (v + 5))
        , \ v -> (v <. 0, A2_ (v <. -3) (v - 5))
        ]

ex5 :: E Int -> Estate (E Bool)
ex5 x = matchM x $ \case
    A2 b n -> pure $ n ==. n ||. b
    A1 n   -> match n $ \case
        A1 _s   -> n >. 1
        A2 t _y -> t

-- Methods to fake constructors (constructor-looking things that behave like
-- functions):

-- Pattern synonyms:
pattern A1_ :: E Int -> Estate A
pattern A1_ x <- (error (errMsg "A1_") -> x)
  where
    A1_ a = do
        t1 <- newFieldTag
        pure $ A1 (t1 a)

pattern A2_ :: E Bool -> E Int -> Estate A
pattern A2_ x y <- (error (errMsg "A2_") -> (x, y))
  where
    A2_ a b = do
        t1 <- newFieldTag
        t2 <- newFieldTag
        pure $ A2 (t1 a) (t2 b)

errMsg :: String -> String
errMsg name = concat
    [ "Pattern synonym `", name, "` can only be used for construction,"
    , " not for destructuring."
    ]

-- Or, underscore-prefixed functions.

_A1 :: E Int -> Estate A
_A1 n = do
    t1 <- newFieldTag
    pure $ A1 (t1 n)

_A2 :: E Bool -> E Int -> Estate A
_A2 x y = do
    t1 <- newFieldTag
    t2 <- newFieldTag
    pure $ A2 (t1 x) (t2 y)
