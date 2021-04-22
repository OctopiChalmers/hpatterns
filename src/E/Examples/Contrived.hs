{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Showcase what happens without the smart constructors.

module E.Examples.Contrived where

import E.Lang


contrived :: E Int -> Estate (E Bool)
contrived v = match v $ \case
    T1 n -> n * n >. n
    T2   -> valE False

data T = T1 (E Int) | T2

instance Partition Int T where
    partition =
        [ \ v -> (v >=. 0, pure $ T1 (v * 10 - 2 * 10 - 2))
        , \ v -> (v <.  0, pure $ T2)
        ]
