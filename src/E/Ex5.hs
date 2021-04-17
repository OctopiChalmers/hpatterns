{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module E.Ex5 where

import Generics.SOP

import E.Core
import E.TH (deriveDummies)

import qualified GHC.Generics as GG (Generic)


data A
    = A1 (E Int)
    | A2 (E Bool) (E Int)
    deriving (GG.Generic, Generic)
deriveDummies ''A

instance Partition A Int where
    partition =
        [ \ v -> (v >. 0, A1 (v + 5))
        , \ v -> (v <. 0, A2 (valE False) (v - 5))
        ]

pattern A1_ :: E Int -> SOP Tag (Code A)
pattern A1_ x <- (a1_ -> Just x)

pattern A2_ :: E Bool -> E Int -> SOP Tag (Code A)
pattern A2_ a b <- (a2_ -> Just (a, b))
{-# COMPLETE A1_, A2_ #-}

a1_ :: SOP Tag (Code A) -> Maybe (E Int)
a1_ (SOP (Z (Tag s a :* Nil))) = Just (ERef s a)
a1_ _ = Nothing

a2_ :: SOP Tag (Code A) -> Maybe (E Bool, E Int)
a2_ (SOP (S (Z (Tag s1 a :* Tag s2 b :* Nil)))) = Just (ERef s1 a, ERef s2 b)
a2_ _ = Nothing

ex5 :: E Int -> Estate (E Int)
ex5 x = pm @A x $ \case
    A1_ n    -> n + 1
    A2_ _b n -> n + n
