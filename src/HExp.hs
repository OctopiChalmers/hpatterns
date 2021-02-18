{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeApplications       #-}

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

    HWhen  :: (Show a, Show b, Eq b)
        => HExp a       -- ^ Return when condition is true
        -> (HExp b, b)  -- ^ Condition is 'When HExp b === b'
        -> HExp a       -- ^ Default value if condition is false
        -> HExp a

    -- HMerge :: (Show a, Show b, Matches a b)
    HMerge :: (Show a, Show b)
        => HExp a       -- ^ Scrutinee
        -> [Match a b]  -- ^ Matches (pattern -> body)
        -> HExp b

    HVar :: Name -> HExp a

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

-- What to do when there isn't a match? I.e. when the "stream" would not
-- produce a value?
hwhen :: (Show a, Show b, Eq b) => HExp a -> (HExp b, b) -> HExp a -> HExp a
hwhen = HWhen

{- | Representation of a case-of expression. Scrutinee is of type 'a', return
value is of type 'b'.
-}
-- hmerge :: (Show a, Show b, Matches a b) => HExp a -> [Match a b] -> HExp b
hmerge :: (Show a, Show b) => HExp a -> [Match a b] -> HExp b
hmerge = HMerge

-- Maybe this isn't really possible in Haski, when using actual streams.
-- Trying to implement this function seems bad, semantics are very unclear.
next :: HExp a -> a
next = \case
    HFby x e -> x
    HVal x   -> x

    HWhen e1 (e2, x) def
        | next e2 == x -> next e1
        | otherwise -> next def

    -- What should the behaviour be here?
    HMerge e branches -> undefined

--
-- * Match representation
--

data Match a b where
    -- Left side is the pattern, right side is the body that will be "run"
    -- when the corresponding partition is matched. Seems more sensible
    -- than ConsMatch
    PartitionMatch :: Partable f x
        => (f x, HExp y) -> Match x y

    -- Left side is some constructor, right side can use HPVar to refer to
    -- arguments. Basically, manually writing the representation of a case
    -- match :( Possible to enforce something like "HPVar is only allowed"
    -- inside a ConsMatch"? Some constraint on this constructor?
    ConsMatch :: (x, HExp y) -> Match x y

deriving instance (Show a, Show b) => Show (Match a b)

class Matching a pat where
    match :: (Show a, Show b) =>
           HExp a           -- ^ Scrutinee
        -> (pat -> HExp b)  -- ^ Case analysis function
        -> HExp b           -- ^ Return an HMerge

--          instance ConsType f => Matching a (f a) where
-- How to work around this? ^ We want some 'match' behavior when f a
-- is a constructor, and some other when it is a partition type. Maybe we
-- need an actual type (with ConsType/Partable constraint), so that these
-- instances work out.

-- Also, how do we solve the very ugly thing we do with ConsType right now?
-- Can't figure out a way to allow the user to write "normal" case-of
-- expressions. Maybe have the user provide an additional list of
-- patterns/constructors/whatever when calling 'match'? In that case,
-- the function arity will differ depending on type... Type families?

-- | Match instance for partition patterns.
instance Partable f a => Matching a (f a) where
    match :: forall b . (Show a, Show b) =>
           HExp a
        -> (f a -> HExp b)
        -> HExp b
    match e caseAnalyze = hmerge e matches
      where
        pats :: [f a]
        pats = [minBound ..]

        bodies :: [HExp b]
        bodies = map caseAnalyze pats

        matches :: [Match a b]
        matches = zipWith (curry PartitionMatch) pats bodies

--
-- * Constructor patterns
--

newtype OutName = OutName String
type Name = String
type Hiska = ST.State Env
data Env = Env
    { envVars :: M.Map Name OutName
    , envSeed :: Int
    }

newVar :: Hiska Name
newVar = do
    freshName <- ("v" ++) . show <$> ST.gets envSeed
    ST.modify (\ env -> env{ envSeed = envSeed env + 1 })
    return freshName

bind :: Name -> Hiska ()
bind s = do
    var <- newVar
    ST.modify (\ env -> env{ envVars = M.insert var (OutName s) (envVars env) })

data SumConstruct a = SumConstruct Name [Name]

class ConsType f where
    consRep :: f a -> Hiska (SumConstruct a)

-- Dummy types
data S a = S a
data T a = T1 a | T2

instance ConsType S where
    consRep (S a) = do
        v <- newVar
        bind v
        return $ SumConstruct "S" [v]

instance ConsType T where
    consRep (T1 a) = do
        v <- newVar
        bind v
        return $ SumConstruct "T1" [v]
    consRep T2 = do
        v <- newVar
        bind v
        return $ SumConstruct "T2" []

initEnv :: Env
initEnv = Env
    { envVars = M.empty
    , envSeed = 0
    }

--
-- * Partition patterns
--

-- | Class for types which can be partitioned into a bounded/enumerable type.
class (Bounded (f a), Enum (f a), Show (f a)) => Partable f a where
    -- | Needs a function to determine how to partition the type, i.e.
    -- convert from values to patterns.
    part :: a -> f a

-- Consider rewriting as an GADT?
data Num a => PatSign a = Pos | Zero | Neg
    deriving (Bounded, Enum, Show)

instance (Ord a, Num a) => Partable PatSign a where
    part :: a -> PatSign a
    part x
        | x > 0     = Pos
        | x < 0     = Neg
        | otherwise = Zero

data Num a => PatParity a = Even | Odd
    deriving (Bounded, Enum, Show)

instance Integral a => Partable PatParity a where
    part :: a -> PatParity a
    part x = if even x then Even else Odd

data IsText a => PatAscii a = Ascii | Other
    deriving (Bounded, Enum, Show)

-- Can we get this behaviour in some other way? I.e. "We can only instantiate
-- PatAscii with String"
class IsText a
instance IsText String

instance Partable PatAscii String where
    part :: String -> PatAscii String
    part xs = if all isAscii xs then Ascii else Other

-- Trivial instances for types that are already finite and enumerable
-- TODO: We don't actually want to write like this though, types like Bool
-- and Int8 should ideally fit in seamlessly.
-- Solution? Maybe some more type wrangling so that certain types get proper
-- default behaviour (that doesn't require using Identity constructor)?

instance Partable Identity Bool where
    part = Identity

instance Partable Identity Int8 where
    part = Identity

instance Partable Identity () where
    part = Identity

--
-- * Misc
--

serialize :: Show a => HExp a -> String
serialize = \case
    HVar s -> s

    HVal v -> show v

    HFby v e -> "(" <> show v <> " fby " <> serialize e <> ")"

    HMerge e branches ->
        "(merge "
        <> "(" <> serialize e <> ")" <> " "
        <> sBranches branches
        <> ")"

    c -> error $ "serialize for `" <> show c <> "` not yet implemented"
  where
    sBranches :: (Show a, Show b) => [Match a b] -> String
    sBranches = unwords . map sCase

    sCase :: (Show a, Show b) => Match a b -> String
    sCase = \case
        PartitionMatch (p, e) -> "(" <> show p <> " -> " <> serialize e <> ")"
        ConsMatch (c, e)      -> "(" <> show c <> " -> " <> serialize e <> ")"

-- Test programs

testSign :: Float -> HExp Int
testSign x = hval x `match` inspect
  where
    inspect :: PatSign Float -> HExp Int
    inspect pf = hval $ case pf of
        Pos  -> 1
        Neg  -> -1
        Zero -> 0

testAscii :: String -> HExp Bool
testAscii s1 = hval s1 `match` inspect
  where
    inspect :: PatAscii String -> HExp Bool
    inspect s = hval $ case s of
        Ascii -> True
        Other -> False

testNot :: Bool -> HExp Bool
testNot b = hval b `match` inspect
  where
    -- Ugly use of Identity constructor is required currently
    inspect :: Identity Bool -> HExp Bool
    inspect (Identity b') = hval (not b')

testUnit :: String -> HExp Bool
testUnit s = match @() (HVar s) inspect
  where
    inspect :: Identity () -> HExp Bool
    inspect _ = hval True

-- Take some variable?
testSign2 :: HExp Int
testSign2 = match @Int8 (HVar "x") inspect
  where
    inspect :: PatSign Int8 -> HExp Int
    inspect pf = hval $ case pf of
        Pos  -> 1
        Neg  -> -1
        Zero -> 0
