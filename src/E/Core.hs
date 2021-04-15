{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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


--
-- * Main data type
--

data E a where
    -- Constructors for pattern matching.
    ESym :: String -> E a
    ECase :: (Partition p a, CType a, CType b)
        => Scrut a
        -> [Match p b]
        -> E b

    -- Straightforward operators.

    EVal :: a -> E a
    EVar :: String -> E a

    EAdd :: (Num a) =>          E a -> E a -> E a
    EMul :: (Num a) =>          E a -> E a -> E a
    ESub :: (Num a) =>          E a -> E a -> E a
    EDiv :: (Fractional a) =>   E a -> E a -> E a
    EGt  :: (Num a, CType a) => E a -> E a -> E Bool
    ELt  :: (Num a, CType a) => E a -> E a -> E Bool
    EGte :: (Num a, CType a) => E a -> E a -> E Bool
    ELte :: (Num a, CType a) => E a -> E a -> E Bool
    EEq  :: (Eq a, CType a) =>  E a -> E a -> E Bool
    ENot ::                     E Bool -> E Bool
    EAnd ::                     E Bool -> E Bool -> E Bool
    EOr  ::                     E Bool -> E Bool -> E Bool

    ECFloorInt    :: E Double -> E Int
    ECFloorDouble :: E Double -> E Double

data Scrut a = Scrut (E a) String

data Match p b where
    Match :: forall p b . CType b
        => E Bool
        -> E b
        -> Match p b

-- ** Helpful instances for 'E' for better ergonomics.

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

-- ** Auxiliary stuff

type Estate = St.State Int

runEstate :: Estate a -> a
runEstate x = St.evalState x 0

-- | Return a unique identifier and increment the counter in state.
freshId :: Estate String
freshId = do
    newId <- ("coreId" ++) . show <$> St.get
    St.modify'(+ 1)
    pure newId

--
-- * Pattern matching
--

class Generic p => Partition p a where
    partition :: [E a -> (E Bool, p)]

match :: forall p a b . (Partition p a, CType a, CType b)
    => E a             -- ^ Scrutinee.
    -> (Rep p -> E b)  -- ^ Function applied to the ADT (representation).
    -> Estate (E b)
match s f = do
    scrutVar <- freshId

    let branches = map ($ ESym scrutVar) $ partition @p @a
    let matches = map (\ (cond, t) -> Match @p @b cond (f $ from t)) branches

    pure $ ECase (Scrut s scrutVar) matches

matchM :: forall p a b . (Partition p a, CType a, CType b)
    => E a
    -- ^ Scrutinee.
    -> (Rep p -> Estate (E b))
    -- ^ Function applied to the ADT (representation).
    -> Estate (E b)
matchM s f = do
    scrutVar <- freshId

    let branches = map ($ ESym scrutVar) $ partition @p @a
    matches <- mapM (\ (cond, adt) -> do
        body <- f (from adt)
        pure $ Match @p @b cond body) branches

    pure $ ECase (Scrut s scrutVar) matches

match2 :: forall p a b . (Partition p a, CType a, CType b)
    => E a
    -> (p -> E b)
    -> Estate (E b)
match2 s f = do
    scrutVar <- freshId

    let branches = map ($ ESym scrutVar) $ partition @p @a
    let matches = map (\ (cond, t) -> Match @p @b cond (f t)) branches

    pure $ ECase (Scrut s scrutVar) matches

--
-- * Straightforward operators
--

infix 4 >.
(>.) :: (Num a, CType a) => E a -> E a -> E Bool
(>.) = EGt

infix 4 >=.
(>=.) :: (Num a, CType a) => E a -> E a -> E Bool
(>=.) = EGte

infix 4 <.
(<.) :: (Num a, CType a) => E a -> E a -> E Bool
(<.) = ELt

infix 4 <=.
(<=.) :: (Num a, CType a) => E a -> E a -> E Bool
(<=.) = ELte

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
