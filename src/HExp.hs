{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HExp where

import Data.Char (isAscii)
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8)

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as M


-- | Main data type.
data HExp a where
    HVal   ::
        a -> HExp a

    HFby   ::
        a -> HExp a -> HExp a

    HMergePart :: (Show a, Partable p a)
        => HExp a           -- ^ Scrutinee
        -> [(p a, HExp b)]  -- ^ Matches (pattern -> body)
        -> HExp b


    HCons1
        :: String  -- ^ Constructor name
        -> HExp a  -- ^ Variable (should always be a HPVar (???))
        -> HExp a

    -- Representation of a case-of expression with a single branch.
    -- TODO: In future, expand to two branches (which can then
    --       model multiple branches).
    HCase :: (Show a, ProdType a)
        => HExp a  -- ^ Scrutinee
        -> HExp b  -- ^ RHS body; uses PVar to refer to components of
                   -- the scrutinee.
        -> HExp b

    HPVar :: HExp a
    HVar :: String -> HExp a

    HAdd :: HExp a -> HExp a -> HExp a
    HMul :: HExp a -> HExp a -> HExp a

    HNeg :: HExp a -> HExp a

    HEq  :: Show a =>
        HExp a -> HExp a -> HExp Bool
deriving instance Show a => Show (HExp a)

-- | Definition of numeric operators on HExps.
instance Num a => Num (HExp a) where
    e1 + e2       = HAdd e1 e2
    e1 * e2       = HMul e1 e2
    fromInteger e = HVal $ fromInteger e
    -- abs n         = HAbs n
    -- signum c      = error "TODO"
    -- negate c      = HNeg c

--
-- * Patterns for product types
--

-- | Create a case-of expression, with a product type scrutinee.
case1 ::
    forall a b .
    ( ProdType a
    , Show a
    )
    => a
    -- ^ Scrutinee. Might make more sense as an HExp a, but this is
    -- simpler for now.

    -> (HExp a -> HExp b)
    -- ^ This function must be representable in the expression language.
    -- Also, we make the assumption that the argument to the function is
    -- exactly a HPVar, even though is not visible in this type (yet?)
    -- TODO: How to ensure this via types?

    -> HExp b
case1 scrut f = HCase theCons (f theVar)
  where
    theVar :: HExp a
    theVar = case args scrut of
        [_] -> HPVar
        _ -> error "lololol"

    theCons :: HExp a
    theCons = HCons1 (consName scrut) HPVar

newtype C = C Int8
    deriving (Show)
instance ProdType C where
    consName _ = "C"
    args (C n) = [ConsArg TInt8 n]

newtype B = B Bool
    deriving (Show)
instance ProdType B where
    consName :: B -> String
    consName _ = "B"

    args :: B -> ConsArgs B
    args (B b) = [ConsArg TBool b]

-- Stolen/"inspired" from "Compiling an Haskell EDSL to C" by Dedden, F.H. 2018
-- | Class for representing product types; single constructor only for now.
class ProdType a where
    -- | We need to be able to get the name of the constructor.
    consName :: a -> String

    args :: a -> ConsArgs a

-- | Supported types as constructors.
data TypeRepr :: * -> * where
    TBool :: TypeRepr Bool
    TInt8 :: TypeRepr Int8

    -- To support nested product types.
    TProdType :: (ProdType s) => s -> TypeRepr s

-- We use this to model heterogenous lists.
type ConsArgs a = [ConsArg]
data ConsArg = forall a . Show a =>
    ConsArg
        (TypeRepr a)  -- ^ Type of argument
        a             -- ^ Value of argument

--
-- * Partition patterns
--

-- | Class for types which can be partitioned into a bounded/enumerable type.
class (Bounded (p a), Enum (p a), Show (p a)) => Partable p a where
    {- | Instances for partable types need a function to determine how to
    partition the type, i.e. convert from values of that type to
    a finite number of patterns (zero-argument constructors, basically).
    -}
    partition :: a -> p a

    {- | We also need to know how to represent the patterns in our
    expression language.
    -}
    toHExp :: p a -> HExp a

    {- Essentially, what it means to be a partable type f a is:
    * We have a definition of how to put all values of f a into "buckets".
      While the type a itself might not be finite, the number of buckets is.
    * We have a definition of how to represent these buckets, and the
      conditions for placing values into these buckets, as Haski (and
      through the backend, C).
    -}

hmatchPart ::
    forall a b p .
    ( Show a
    , Show b
    , Partable p a
    )
    => HExp a           -- ^ Scrutinee
    -> (p a -> HExp b)  -- ^ Matching function
    -> HExp b           -- ^ Return an HMerge
hmatchPart e f = hmergePart e branches
  where
    pats :: [p a]
    pats = [minBound ..]

    bodies :: [HExp b]
    bodies = map f pats

    branches :: [(p a, HExp b)]
    branches = zip pats bodies

-- Consider rewriting as an GADT?
data Num a => PatSign a = Pos | Zero | Neg
    deriving (Bounded, Enum, Show)

instance (Ord a, Num a) => Partable PatSign a where
    partition :: a -> PatSign a
    partition x
        | x > 0     = Pos
        | x < 0     = Neg
        | otherwise = Zero

data Num a => PatParity a = Even | Odd
    deriving (Bounded, Enum, Show)

instance Integral a => Partable PatParity a where
    partition :: a -> PatParity a
    partition x = if even x then Even else Odd

data IsText a => PatAscii a = Ascii | Other
    deriving (Bounded, Enum, Show)

-- Can we get this behaviour in some other way? I.e. "We can only instantiate
-- PatAscii with String"
class IsText a
instance IsText String

instance Partable PatAscii String where
    partition :: String -> PatAscii String
    partition xs = if all isAscii xs then Ascii else Other

-- Trivial instances for types that are already finite and enumerable
-- TODO: We don't actually want to write like this though, types like Bool
-- and Int8 should ideally fit in seamlessly.
-- Solution? Maybe some more type wrangling so that certain types get proper
-- default behaviour (that doesn't require using Identity constructor)?

instance Partable Identity Bool where
    partition = Identity

instance Partable Identity Int8 where
    partition = Identity

instance Partable Identity () where
    partition = Identity

--
-- * Combinators and primitives
--

-- | Lift a value into an expression.
hval :: a -> HExp a
hval = HVal

-- | Constructor. Compare to 'cons'.
hfby :: a -> HExp a -> HExp a
hfby = HFby

-- | Representation of a case-of expression for partition patterns.
hmergePart :: (Show a, Show b, Partable p a)
    => HExp a           -- ^ Scrutinee
    -> [(p a, HExp b)]  -- ^ List of matches (partition -> body)
    -> HExp b
hmergePart = HMergePart

hnot :: HExp a -> HExp a
hnot = HNeg

--
-- * Misc
--

-- | Convert HPVars into into normal HVars.
-- Uses only one variable name for now.
rename :: Show a =>HExp a -> HExp a
rename exp = case exp of
    HCase scrut body -> HCase (rename scrut) (rename body)
    HCons1 name HPVar -> HCons1 name (HVar varName)
    HPVar -> HVar varName

    HAdd e1 e2 -> HAdd (rename e1) (rename e2)
    HMul e1 e2 -> HMul (rename e1) (rename e2)
    HNeg e -> HNeg (rename e)
    HEq e1 e2 -> HEq (rename e1) (rename e2)

    -- In the future, sub-expressions in these should be inspected for
    -- renaming also.
    HFby{} -> exp
    HMergePart{} -> exp

    HVal{} -> exp
    HPVar{} -> exp

    _ -> error $ "rename: unexpected constructor `" <> show exp <> "`"
  where
    varName :: String
    varName = "x"


serialize :: Show a => HExp a -> String
serialize = \case
    HVal v -> show v

    HFby v e -> "(" <> show v <> " fby " <> serialize e <> ")"

    HMergePart e branches ->
        "(merge "
        <> "(" <> serialize e <> ")" <> " "
        <> sBranches branches
        <> ")"

    c -> error $ "serialize for `" <> show c <> "` not yet implemented"
  where
    sBranches :: (Show a, Show b, Partable p a) => [(p a, HExp b)] -> String
    sBranches = unwords . map sCase

    sCase :: (Show a, Show b, Partable p a) => (p a, HExp b) -> String
    sCase (pat, e) = "(" <> show pat <> " -> " <> serialize e <> ")"
