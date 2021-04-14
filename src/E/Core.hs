{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module E.Core where

import Data.SOP
import Generics.SOP

import E.CTypes

import qualified Control.Monad.State.Strict as St
import qualified GHC.Generics as GG


data Sig = Pos (E Int) | Neg
    deriving GG.Generic
instance Generic Sig

class Generic p => Partition p a where
    partition :: [E a -> (E Bool, p)]

instance Partition Sig Double where
    partition =
        [ \ v -> (v >. 0, Pos $ floorIntE v)
        , \ v -> (v >. 0, Neg)
        ]

case' :: forall p a b . Partition p a
    => E a
    -> (Rep p -> E b)
    -> Estate (E b)
case' s f = do
    scrutVar <- freshId

    let branches = map ($ ESym scrutVar) $ partition @p @a
    let matches = map (\ (cond, t) -> Match @p @b cond (from t) (f $ from t)) branches

    pure $ ECase (Scrut s scrutVar) matches

--
-- * Main data type
--

data Scrut a = Scrut (E a) String

data Match p b where
    Match
        :: E Bool
        -> (Rep p)
        -> E b
        -> Match p b

data E a where
    EVal :: a -> E a
    EVar :: String -> E a

    ESym :: String -> E a
    ECase :: Partition p a => Scrut a -> [Match p b] -> E b

    EAdd :: (Num a) =>          E a -> E a -> E a
    EMul :: (Num a) =>          E a -> E a -> E a
    ESub :: (Num a) =>          E a -> E a -> E a
    EDiv :: (Fractional a) =>   E a -> E a -> E a
    EGt  :: (Num a, CType a) => E a -> E a -> E Bool
    ELt  :: (Num a, CType a) => E a -> E a -> E Bool
    EEq  :: (Eq a, CType a) =>  E a -> E a -> E Bool
    ENot ::                     E Bool -> E Bool
    EAnd ::                     E Bool -> E Bool -> E Bool
    EOr  ::                     E Bool -> E Bool -> E Bool

    -- C stuff

    ECFloorInt    :: E Double -> E Int
    ECFloorDouble :: E Double -> E Double

instance (Num a) => Num (E a) where
    (+) = EAdd
    (*) = EMul
    (-) = ESub
    fromInteger n = EVal (fromInteger n)
    abs = error "not implemented"
    signum = error "not implemented"

instance (Fractional a) => Fractional (E a) where
    fromRational r = EVal (fromRational r)
    (/) = EDiv

infix 4 >.
(>.) :: (Num a, CType a) => E a -> E a -> E Bool
(>.) = EGt

infix 4 <.
(<.) :: (Num a, CType a) => E a -> E a -> E Bool
(<.) = ELt

infix 4 ==.
(==.) :: (Eq a, CType a) => E a -> E a -> E Bool
(==.) = EEq

infix 4 /=.
(/=.) :: (Eq a, CType a) => E a -> E a -> E Bool
x /=. y = notE (x `EEq` y)

infixr 3 &&.
(&&.) :: E Bool -> E Bool -> E Bool
(&&.) = EAnd

infixr 2 ||.
(||.) :: E Bool -> E Bool -> E Bool
(||.) = EOr

(!.) :: E Bool -> E Bool
(!.) = notE

notE :: E Bool -> E Bool
notE = ENot

varE :: String -> E a
varE = EVar

valE :: a -> E a
valE = EVal


-- * Uses <math.h>

floorDoubleE :: E Double -> E Double
floorDoubleE = ECFloorDouble

floorIntE :: E Double -> E Int
floorIntE = ECFloorInt

fracPartE :: E Double -> E Double
fracPartE d = d - floorDoubleE d


-- * Auxiliary stuff

type Estate = St.State Int

runEstate :: Estate a -> a
runEstate x = St.evalState x 0

-- | Return a unique identifier and increment the counter in state.
freshId :: Estate String
freshId = do
    newId <- ("coreId" ++) . show <$> St.get
    St.modify'(+ 1)
    pure newId
