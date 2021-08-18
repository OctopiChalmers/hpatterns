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
    , enumIdPartitions
    , caseof
    , caseofM

    -- * Basic operators
    , (>.)
    , (>=.)
    , (<.)
    , (<=.)
    , (==.)
    , (/=.)
    , (&&.)
    , (||.)
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
import Data.Int (Int8)
import Data.Proxy (Proxy (..))

import E.Core
import E.CTypes


--
-- * Pattern matching
--

{- | A @Partition a p@ instance for defines two things:

* How to construct a value of type @p@ given an input value of type @E a@.

* For each constructor, a predicate on an input value of type @E a@ that
determines when to "choose" that branch/constructor when pattern matching.

These two properties are used in the 'caseof' combinator to perform matching.
-}
class Partition a p where
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

{- | Helper function for identity instances, for 'FinEnum' types.

Since a 'Partition' instance is necessary to use 'caseof,
it was a bit inconvenient for non-"normal" sum types,
e.g. Int8. The helper function 'enumIdPartitions' acts similar
to The Trick used in Haski, enumerating through all values of the input
type of the 'Partition' to generate literal constructor cases.

By implementing 'partition' with this for instances such as
'Partition Int8 Int8', we can make better use of Haskell features (such as
guards) when matching on supported values (see below).

For example, when matching on a value of type Int8:

> caseof n $ \case
>     0 -> 0
>     1 -> 99
>     n | n > 0 -> (-n)
>       | n < 0 -> n
-}
enumIdPartitions :: forall a . (Enum a, Eq a, Bounded a, CType a)
    => [E a -> (E Bool, Estate a)]
enumIdPartitions = map build [minBound .. maxBound]
  where
    build :: a -> E a -> (E Bool, Estate a)
    build x = \v -> (v ==. valE x, pure x)

{- Examples of useful Partition instances that make use of 'enumIdPartitions'
to allow literal pattern matching (and the use of guards etc.)

Note that there is no instance provided for integers of sizes higher than
8 bits. Larger sizes would work, but the generated code gets huge.
-}
instance Partition Bool Bool where
    partition = enumIdPartitions
instance Partition Int8 Int8 where
    partition = enumIdPartitions


{- | Partition a scrutinee and apply a pattern matching function on the
partition type.

Example, for @'Partition' T Int@:

> data T
>     = T1 (E Int)
>     | T2 (E Bool) (E Int)
>
> ex5 :: E Int -> Estate (E Bool)
> ex5 x = caseof x $ \case
>     T1 n   -> n >. 1
>     T2 t n -> t ||. n >. 100
-}
caseof :: forall p a b . (Partition a p, CType a, CType b)
    => E a
    -- ^ Scrutinee; the value being pattern matched on, after conversion
    -- according to the 'Partition' instance.
    -> (p -> E b)
    -- ^ Function to apply to the scrutinee.
    -> Estate (E b)
caseof s f = do
    -- Generate a variable name to differentiate this scrutinee from others
    -- during compilation.
    scrutVar <- newScrutId
    -- Apply the partitioning on symbolic variables referring to the scrutinee.
    -- When the user pattern matches and uses one of these variables, they
    -- will have the transformation applied by the Partition instance.
    let branches = map ($ ESym scrutVar) $ partition @a @p
    taggedBranches <- mapM computeTag branches

    pure $ ECase (Scrut s scrutVar) $ map mkMatch taggedBranches
  where
    computeTag :: (E Bool, Estate p) -> Estate (E Bool, p)
    computeTag (cond, p) = (cond, ) <$> p

    mkMatch :: (E Bool, p) -> Match p b
    mkMatch (cond, p) = Match @p @b cond (f p)

{- | Same as caseof but the pattern matching function is monadic (used
for nested matches).

Example, for @'Partition' T Int@:

> data T
>     = T1 (E Int)
>     | T2 (E Bool) (E Int)
>
> ex5 :: E Int -> Estate (E Bool)
> ex5 x = caseofM x $ \case
>     T2 b n -> pure $ n ==. n ||. b
>     T1 n   -> caseof n $ \case
>         T1 _   -> n >. 1
>         T2 t _ -> t
-}
caseofM :: forall p a b . (Partition a p, CType a, CType b)
    => E a
    -> (p -> Estate (E b))
    -> Estate (E b)
caseofM s f = do
    scrutCount <- newScrutId
    let branches = map ($ ESym scrutCount) $ partition @a @p
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
(/=.) = ENeq

infixr 3 &&.
(&&.) :: E Bool -> E Bool -> E Bool
(&&.) = EAnd

infixr 2 ||.
(||.) :: E Bool -> E Bool -> E Bool
(||.) = EOr

notE :: E Bool -> E Bool
notE = ENot

varE :: String -> E a
varE = EVar

valE :: a -> E a
valE = EVal

-- ** Uses <math.h>

-- | Round downwards, i.e. __not__ towards zero.
floorDoubleE :: E Double -> E Double
floorDoubleE = ECFloorDouble

-- | Round downwards, i.e. __not__ towards zero.
floorIntE :: E Double -> E Int
floorIntE = ECFloorInt

{- | Return the fractional part of a @Double@ (the part after the decimal).

> fracPartE 123.789 == 0.789
-}
fracPartE :: E Double -> E Double
fracPartE d = d - floorDoubleE d

-- ** UNSAFE bitwise operations

-- This business should be handled using some safer abstraction in the future;
-- the capability is here to enable some examples, but these operators are
-- basically direct translations of their corresponding C variants.

-- | @testBitE n v@ returns True if the @n@th bit of the value @v@ is set.
testBitE :: (Bits a, Num a, CType a) => E Int -> E a -> E Bool
testBitE n v = bitE n &. v /=. 0

-- | @zeroBitsE@ is a value with all bits clear. Kind of redundant.
zeroBitsE :: (Bits a, Num a) => E a
zeroBitsE = 0

-- | @bitE n@ is a value with only the @n@th bit set, and all other bits clear.
bitE :: (Bits a, Num a) => E Int -> E a
bitE n = 1 <<. n

-- | Bitwise right shift.
infix 4 >>.
(>>.) :: Bits a => E a -> E Int -> E a
(>>.) = EShiftR

-- | Bitwise left shift.
infix 4 <<.
(<<.) :: Bits a => E a -> E Int -> E a
(<<.) = EShiftL

-- | Bitwise AND.
infix 5 &.
(&.) :: Bits a => E a -> E a -> E a
(&.) = EBitAnd

-- ** UNSAFE in general

castE :: forall a b . (CType a, CType b) => E a -> E b
castE e = ECast e (Proxy @b)
