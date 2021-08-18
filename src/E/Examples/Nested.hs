{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Using nested applications of 'caseof'.

module E.Examples.Nested where

import E.Lang
import E.TH


nested :: E Int -> Estate (E Int)
nested v = caseofM v $ \case
    T2   -> pure 98
    T1 x -> caseofM (x + 1000) $ \case
        T2   -> pure 99
        T1 y -> pure (x + 2 + y)

data T = T1 (E Int) | T2
$(mkConstructors ''T)

instance Partition Int T where
    partition = [ \v -> (v >=. 0, _T1 (v * 2))
                , \v -> (v <. 0, _T2)
                ]

