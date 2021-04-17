{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module E.Core where

import Data.Bifunctor
import Data.SOP
import Data.Kind
import Generics.SOP

import E.CTypes

import qualified Control.Monad.State.Strict as St


type TaggedADT a = SOP Tag (Code a)

data Tag a = Tag String a

tag :: (All SListI xss) => String -> SOP I xss -> SOP Tag xss
tag s sop = hmap (\ (I a) -> Tag s a) sop

pm :: forall p a b . (Partition p a, CType a, CType b)
    => E a
    -> (SOP Tag (Code p) -> E b)
    -> Estate (E b)
pm e f = do
    -- Generate a variable name to differentiate this scrutinee from others
    -- during compilation.
    scrutVar <- freshId
    -- Apply the partitioning on symbolic variables referring to the scrutinee.
    -- When the user pattern matches and uses one of these variables, they
    -- will have the transformation applied by the Partition instance.
    let branches = map ($ ESym scrutVar) $ partition @p @a
    let x = map (second (tag scrutVar . from)) branches

    pure $ ECase2 @p @a (Scrut e scrutVar) (map mkTriple x)
  where
    mkTriple :: (E Bool, TaggedADT p) -> (E Bool, TaggedADT p, E b)
    mkTriple (cond, p) = (cond, p, (f p))
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

    ECase2 :: (Partition p a, CType a, CType b)
        => Scrut a
        -> [(E Bool, TaggedADT p, E b)]
        -> E b

    ERef :: String -> E a -> E a

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

{- | Partitioning a scrutinee and apply a pattern matching function on the
partition type.
-}
match :: forall p a b . (Partition p a, CType a, CType b)
    => E a
    -> (p -> E b)
    -> Estate (E b)
match s f = do
    -- Generate a variable name to differentiate this scrutinee from others
    -- during compilation.
    scrutVar <- freshId
    -- Apply the partitioning on symbolic variables referring to the scrutinee.
    -- When the user pattern matches and uses one of these variables, they
    -- will have the transformation applied by the Partition instance.
    let branches = map ($ ESym scrutVar) $ partition @p @a

    pure $ ECase (Scrut s scrutVar) (map mkMatch branches)
  where
    mkMatch :: (E Bool, p) -> Match p b
    mkMatch (cond, p) = Match @p @b cond (f p)

{- | Same as match' but the pattern matching function is be monadic (used
for nested matches).
-}
matchM :: forall p a b . (Partition p a, CType a, CType b)
    => E a
    -> (p -> Estate (E b))
    -> Estate (E b)
matchM s f = do
    scrutVar <- freshId
    let branches = map ($ ESym scrutVar) $ partition @p @a
    ECase (Scrut s scrutVar) <$> mapM mkMatch branches
  where
    mkMatch :: (E Bool, p) -> Estate (Match p b)
    mkMatch (cond, p) = Match @p @b cond <$> f p

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
