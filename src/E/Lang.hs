{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

{- | The user-facing library for building programs in the expression language.

Also exports some needed types and functions from "E.Core".
-}

module E.Lang
    (
    -- * Pattern matching
      Partition (..)
    , match
    , matchM

    -- * Basic operators
    , (>.)
    , (>=.)
    , (<.)
    , (<=.)
    , (==.)
    , (/=.)
    , (&&.)
    , (||.)
    , (!.)
    , notE
    , varE
    , valE

    -- ** Uses \<stdmath.h\>
    , floorDoubleE
    , floorIntE
    , fracPartE

    -- ** UNSAFE bitwise operations
    , testBitE
    , zeroBitsE
    , bitE
    , (>>.)
    , (<<.)
    , (&.)

    -- ** UNSAFE in general
    , castE

    -- * Re-exports from "E.Core"
    , E.Core.E
    , E.Core.Estate
    , E.Core.runEstate
    ) where

import Data.Bits (Bits)
import Data.Proxy (Proxy (..))

import E.Core
import E.CTypes


--
-- * Pattern matching
--

{- | A @Partition p a@ instance for defines two things:

* How to construct a value of type @p@ given an input value of type @E a@.

* For each constructor, a predicate on an input value of type @E a@ that
determines when to "choose" that branch/constructor when pattern matching.

These two properties are used in the 'match' combinator to perform matching.
-}
class Partition p a where
    {- | Return a list of functions that create matches/branches, one for
    each constructor of the partition type @p@. Each function in the list
    takes an input value of type @E a@ and returns a tuple, where:

    * The first element is a predicate on the input, in the expression
    language. The predicate determines whether the body (second element)
    should be run or not in the compiled pattern match.

    * The second element is a constructed value of the partition type @p@,
    whose values may depend on the input value @E a@.

    The idea is to use the predicate expression to determine (depending on the
    input) which constructor of the sum type @p@ to use. This means that
    @partition@ should return one function for each constructor of the type
    @p@, though this is not enforced.
    -}
    partition :: [E a -> (E Bool, Estate p)]

{- | Partition a scrutinee and apply a pattern matching function on the
partition type.

Example, for @'Partition' T Int@:

> data T
>     = T1 (E Int)
>     | T2 (E Bool) (E Int)
>
> ex5 :: E Int -> Estate (E Bool)
> ex5 x = match x $ \case
>     T1 n   -> n >. 1
>     T2 t n -> t ||. n >. 100
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

{- | Same as match but the pattern matching function is monadic (used
for nested matches).

Example, for @'Partition' T Int@:

> data T
>     = T1 (E Int)
>     | T2 (E Bool) (E Int)
>
> ex5 :: E Int -> Estate (E Bool)
> ex5 x = matchM x $ \case
>     T2 b n -> pure $ n ==. n ||. b
>     T1 n   -> match n $ \case
>         T1 _   -> n >. 1
>         T2 t _ -> t
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
-- * Basic operators
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

-- ** Uses <math.h>

floorDoubleE :: E Double -> E Double
floorDoubleE = ECFloorDouble

floorIntE :: E Double -> E Int
floorIntE = ECFloorInt

fracPartE :: E Double -> E Double
fracPartE d = d - floorDoubleE d

-- ** UNSAFE bitwise operations

-- This business should be handled using some safer abstraction in the future;
-- the capability is here to enable some examples, but these operators are
-- basically direct translations of their corresponding C variants.

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

-- ** UNSAFE in general

castE :: forall a b . (CType a, CType b) => E a -> E b
castE e = ECast e (Proxy @b)
