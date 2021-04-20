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
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module E.Core where

import Data.Bits
import Data.Proxy

import E.CTypes (CType)

import qualified Control.Monad.State.Strict as St



--
-- * Main data type
--

data E a where
    -- Constructors for pattern matching.
    ESym :: ScrutId -> E a
    ECase :: (Partition p a, CType a, CType b)
        => Scrut a
        -> [Match p b]
        -> E b

    EField :: ArgId -> E a -> E a

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

    -- UNSAFE bit twiddling stuff; this stuff should be handled separately
    -- in the future maybe. hacky thing to test stuff atm

    ECast   :: (CType a, CType b) => E a -> Proxy b -> E b  -- yikes

    -- EBit    :: (Bits a) => E Int -> E a
    EShiftL :: (Bits a) => E a -> E Int -> E a
    EShiftR :: (Bits a) => E a -> E Int -> E a
    EBitAnd :: (Bits a) => E a -> E a -> E a

data Scrut a = Scrut (E a) ScrutId

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

type Estate = St.State Env

newtype ScrutId = ScrutId Int deriving (Eq, Ord, Show)
newtype FieldId = FieldId Int deriving (Eq, Ord, Show)

{- | An 'ArgId' is an identifier for a single bound field of a deconstructed
value in a pattern match.
-}
data ArgId = ArgId ScrutId FieldId
    deriving (Eq, Ord, Show)

data Env = Env
    { envScrutCount :: Int
    , envFieldCount :: Int
    }

runEstate :: Estate a -> a
runEstate x = St.evalState x initEnv
  where
    initEnv :: Env
    initEnv = Env 0 0

-- | Return a unique identifier and increment the counter in state.
newScrutId :: Estate ScrutId
newScrutId = do
    St.modify' (\ st -> st { envScrutCount = envScrutCount st + 1 })
    ScrutId . envScrutCount <$> St.get

newFieldTag :: Estate (E a -> E a)
newFieldTag = do
    St.modify' (\ st -> st { envFieldCount = envFieldCount st + 1 })
    scrutId <- ScrutId . envScrutCount <$> St.get
    fieldId <- FieldId . envFieldCount <$> St.get
    pure $ EField (ArgId scrutId fieldId)

--
-- * Pattern matching
--

class Partition p a where
    partition :: [E a -> (E Bool, Estate p)]

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
    scrutVar <- newScrutId
    -- Apply the partitioning on symbolic variables referring to the scrutinee.
    -- When the user pattern matches and uses one of these variables, they
    -- will have the transformation applied by the Partition instance.
    let branches = map ($ ESym scrutVar) $ partition @p @a
    taggedBranches <- mapM computeTag branches

    pure $ ECase (Scrut s scrutVar) $ map mkMatch taggedBranches
  where
    computeTag :: (E Bool, Estate p) -> Estate (E Bool, p)
    computeTag (cond, p) = (cond, ) <$> p

    mkMatch :: (E Bool, p) -> Match p b
    mkMatch (cond, p) = Match @p @b cond (f p)

{- | Same as match but the pattern matching function is be monadic (used
for nested matches).
-}
matchM :: forall p a b . (Partition p a, CType a, CType b)
    => E a
    -> (p -> Estate (E b))
    -> Estate (E b)
matchM s f = do
    scrutCount <- newScrutId
    let branches = map ($ ESym scrutCount) $ partition @p @a
    taggedBranches <- mapM computeTag branches
    ECase (Scrut s scrutCount) <$> mapM mkMatch taggedBranches
  where
    computeTag :: (E Bool, Estate p) -> Estate (E Bool, p)
    computeTag (cond, p) = (cond, ) <$> p

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

-- * !!VERY UNSAFE!! Bitwise operations (hacky stuff to test some things)

testBitE :: (Bits a, Num a, CType a) => E Int -> E a -> E Bool
testBitE n v = bitE n &. v ==. 0

zeroBitsE :: (Bits a, Num a) => E a
zeroBitsE = 0

bitE :: (Bits a, Num a) => E Int -> E a
bitE n = 1 <<. n

infix 4 >>.
(>>.) :: Bits a => E a -> E Int -> E a
(>>.) = EShiftR

infix 4 <<.
(<<.) :: Bits a => E a -> E Int -> E a
(<<.) = EShiftL

infix 5 &.
(&.) :: Bits a => E a -> E a -> E a
(&.) = EBitAnd

castE :: forall a b . (CType a, CType b) => E a -> E b
castE e = ECast e (Proxy @b)
