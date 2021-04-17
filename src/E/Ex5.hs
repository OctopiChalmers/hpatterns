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

import Data.Bifunctor
import Generics.SOP

import E.Core
import E.CTypes
import E.Dummies (Dummies (mkDummies))
import E.TH (deriveDummies)

import qualified GHC.Generics as GG (Generic)


data A
    = A1 (E Int)
    | A2 (E Bool) (E Int)
    deriving (GG.Generic, Generic)
deriveDummies ''A

pattern A1_ :: E Int -> SOP Tag (Code A)
pattern A1_ x <-
    ((\ (SOP (Z (Tag s a :* Nil))) -> ERef s a) -> x)

pattern A2_ :: E Bool -> E Int -> SOP Tag (Code A)
pattern A2_ a b <-
    ((\ (SOP (S (Z (Tag s a :* Tag t b :* Nil)))) -> (ERef s a, ERef t b)) -> (a, b))
{-# COMPLETE A1_, A2_ #-}

-- f :: Rep A -> E Int
f x = match x $ \case
    A1_ n    -> n + 1
    A2_ _b n -> n + n



