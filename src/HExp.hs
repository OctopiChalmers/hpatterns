{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module HExp where

import Data.Char (isAscii)
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int8)
import Data.Proxy (Proxy (..))

import qualified Data.List
import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as M


-- | Main data type.
data HExp a where
    HMergePart :: forall p a b .
        ( Show a
        , Show b
        , Partable p a
        )
        => HExp a           -- ^ Scrutinee
        -> [(p a, HExp b)]  -- ^ Matches (pattern -> body)
        -> HExp b

    HCase0 :: forall a b .
        ( Show a
        )
        => HExp a
            -- ^ Scrutinee
        -> [(HExp Bool, HExp b)]
            -- ^ Branches. Consist of a condition (using HPVar to reference
            -- the bound variable) and the body.
        -> HExp b

    HCase2 :: forall a b .
        ( ProdType a
        , Show a
        )
        => HExp a  -- ^ Scrut
        -> HExp b  -- ^ ??? and body
        -> HExp b

    -- Field access for structs.
    HDot :: forall a b .
        ( Show a
        )
        => HExp a  -- ^ Expression of struct
        -> String  -- ^ Name of Field
        -> HExp b

    HNamedExp :: String -> HExp a -> HExp a
    HVar :: String -> HExp a
    HPVar :: HExp a

    -- Constructors below are visible to the user.
    HVal :: (Show a) => a -> HExp a
    HAdd :: Num a => HExp a -> HExp a -> HExp a
    HMul :: Num a => HExp a -> HExp a -> HExp a
    HGt :: (Show a, Num a) => HExp a -> HExp a -> HExp Bool
deriving instance Show a => Show (HExp a)

-- | Definition of numeric operators on HExps.
instance (Show a, Num a) => Num (HExp a) where
    e1 + e2       = HAdd e1 e2
    e1 * e2       = HMul e1 e2
    fromInteger e = HVal $ fromInteger e
    -- abs n         = HAbs n
    -- signum c      = error "TODO"
    -- negate c      = HNeg c

--
-- * Product type representation
--

-- Taken from "Compiling an Haskell EDSL to C" by Dedden, F.H. 2018
-- | Class for representing product types; single constructor only for now.
class ProdType a where
    consName :: a -> String
    args :: a -> ConsArgs a
    arity :: a -> Int
    argNames :: [String]
    structDef :: a -> String

    default
        structDef :: a -> String
    structDef x =
        "struct " <> consName x <> " {\n"
        <> concatMap
            (\ (ConsArg t s _) -> "    " <> showTypeRep t <> " " <> s <> ";\n")
            (args x)
        <> "};\n"


-- | Supported types as constructors.
data TypeRepr a where
    TBool :: TypeRepr Bool
    TInt :: TypeRepr Int

    -- To support nested product types.
    -- TProdType :: (ProdType s) => s -> TypeRepr s

showTypeRep :: TypeRepr a -> String
showTypeRep TBool = "bool"
showTypeRep TInt = "int"

type ConsArgs a = [ConsArg]
data ConsArg = forall a. Show a =>
    ConsArg
        (TypeRepr a)  -- Type of field
        String        -- Name of field
        a             -- Value of field

--
-- * Case
--

-- | Class for types which can be partitioned into a bounded/enumerable type.
class (Bounded (p a), Enum (p a), Show (p a)) => Partable p a where
    {- | Instances for partable types need a function to determine how to
    partition the type, i.e. convert from values of that type to
    a finite number of patterns (zero-argument constructors, basically).
    -}
    partition :: a -> p a

    {- | We also need to know how to represent the patterns as conditions
    in our expression language.
    -}
    toHExp :: p a -> HExp Bool

    -- | The name of the type as an Enum in the generated code.
    enumName :: p a -> String

    {- Essentially, what it means to be a partable type f a is:
    * We have a definition of how to put all values of f a into "buckets".
      While the type a itself might not be finite, the number of buckets is.
    * We have a definition of how to represent these buckets, and the
      conditions for placing values into these buckets, as Haski (and
      through the backend, C).
    -}

hmatchPart ::
    forall p a b .
    ( Show a
    , Show b
    , Partable p a
    )
    => HExp a           -- ^ Scrutinee
    -> (p a -> HExp b)  -- ^ Matching function
    -> HExp b           -- ^ Return an HMerge
hmatchPart e f = HMergePart @p e branches
  where
    pats :: [p a]
    pats = [minBound ..]

    bodies :: [HExp b]
    bodies = map f pats

    branches :: [(p a, HExp b)]
    branches = zip pats bodies

data Num a => PatSign a = Pos | Neg
    deriving (Bounded, Enum, Show)

instance (Show a, Num a, Ord a) => Partable PatSign a where
    partition :: a -> PatSign a
    partition x
        | x >= 0 = Pos
        | x < 0  = Neg

    enumName _ = "Sign"

    toHExp :: PatSign a -> HExp Bool
    toHExp = \case
        Pos -> HGt HPVar (HVal 0)
        Neg -> HGt (HVal 0) HPVar
