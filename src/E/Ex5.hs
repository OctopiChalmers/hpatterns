{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE ViewPatterns            #-}

module E.Ex5 where

import Generics.SOP

import E.Core
import E.Dummies ()
import E.TH (deriveDummies)

import qualified GHC.Generics as GG (Generic)


data A
    = A1 (E Int)
    | A2 (E Bool) (E Int)
    deriving (GG.Generic, Generic)
$(deriveDummies ''A)

pattern A1_ :: E Int -> Rep A
pattern A1_ a <- SOP (Z (I a :* Nil))
pattern A2_ :: E Bool -> E Int -> Rep A
pattern A2_ a b <- SOP (S (Z (I a :* I b :* Nil)))
{-# COMPLETE A1_, A2_ #-}

f :: Rep A -> E Int
f = \case
    A1_ n    -> n + 1
    A2_ _b n -> n + n
