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

    HMergePart :: (Show a, Show b, Partable p a)
        => HExp a           -- ^ Scrutinee
        -> [(p a, HExp b)]  -- ^ Matches (pattern -> body)
        -> HExp b

    {- | For simple product types the purpose of pattern matching is
    simply to deconstruct the value into its constructor and paramenters.
    Therefore, we don't provide a list of branches/matches like with
    partition matching, for example.
    -}
    HMergeProd :: (Show a, Show b, ProdType a)
        => a  -- ^ Scrutinee
        -> HExp b
        -> HExp b

    HVar :: String -> HExp a

    HPVar :: HExp a

deriving instance Show a => Show (HExp a)

--
-- * Combinators
--

-- | Lift a value into an expression.
hval :: a -> HExp a
hval = HVal

-- | Constructor. Compare to 'cons'.
hfby :: a -> HExp a -> HExp a
hfby = HFby

{- | Representation of a case-of expression. Scrutinee is of type 'a', return
value is of type 'b'.
-}
hmergePart :: (Show a, Show b, Partable p a)
    => HExp a
    -> [(p a, HExp b)]
    -> HExp b
hmergePart = HMergePart

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
-- * Constructor patterns
--

hmatchProd ::
    forall a b .
    ( ProdType a
    , Show a
    , Show b
    ) =>

    a ->

    {- | Any constraints on this function? Unfortunately, we cannot just have
    the user refer to the constructor arguments like normal. Rather, they
    need to maybe use the pVar combinator to refer to each argument in
    order? So if type a has a constructor with 2 args, then the user
    can/must use 2 pVars to refer to the arguments? How do we get this
    type safe, i.e. ensure, that the function passed is one that can
    actually be used on the type a? Maybe put more stuff into the
    ProdType class? That way, we could assume more stuff (class methods)
    here in the matching function.
    -}
    (ConsArgs a -> HExp b) ->

    HExp b
hmatchProd scrut f = f (args scrut)

-- If we want to try to generate all possible values of the args, we'd need
-- some function similar to this:
--      extractArgs :: ProdType a => ConsArgs a -> SomeStructure a
-- The return type here depends on the type of a (what the struct looks
-- like), so this seems pretty hard!
-- Functional dependencies? (the output structure/type depends on a)
-- Type families? (should be able to solve the same problem)

tes :: B -> HExp Bool
tes x = x `hmatchProd` inspect
  where
    -- How to make this function type safe? Feels like some level of
    -- type programming is in order, probably
    inspect :: [ConsArg] -> HExp Bool
    inspect [ConsArg TBool b] = hval $ not b

newtype B = B
    { bB :: Bool
    }
    deriving (Show, Bounded, Enum)
instance ProdType B where
    consName :: B -> String
    consName _ = "B"

    args :: B -> ConsArgs B
    args (B b) = [ConsArg TBool b]

-- Stolen/"inspired" from "Compiling an Haskell EDSL to C" by Dedden, F.H. 2018
-- | Class for representing product types; single constructor only for now.
class (Bounded a, Enum a) => ProdType a where
    -- Functional dependencies/type families, for having different output
    -- types? I.e., something like:
    -- class ... => ProdType a out | a -> out  -- output type depends on a
    --     extractArgs :: a -> out
    -- So, for B:
    --     extractArgs :: B -> Bool  -- For a 2D vec, this could be (Int, Int)
    --     extractArgs (B b) = b

    -- type OutType a ::

    -- | We need to be able to get the name of the constructor.
    consName :: a -> String

    args :: a -> ConsArgs a

-- | Supported types as constructors.
data TypeRepr :: * -> * where
    TBool :: TypeRepr Bool
    TInt8 :: TypeRepr Int8

    -- To support nested product types.
    TProdType :: (ProdType s) => s -> TypeRepr s

type ConsArgs a = [ConsArg]

-- How does this type work? How can we define Enum/Bounded
-- instances for it? (Which we would need to generate all
-- possible arguments).
data ConsArg = forall a . (Bounded a, Enum a) =>
    ConsArg
        (TypeRepr a)  -- ^ Type of argument
        a             -- ^ Value of argument

--
-- * Misc
--

serialize :: Show a => HExp a -> String
serialize = \case
    HVar s -> s

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
