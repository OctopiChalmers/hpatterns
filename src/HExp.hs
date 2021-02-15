{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module HExp where

import Data.Char (isAscii)


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

    HMerge :: (Show a, Show b, Partable f a)
        => HExp a           -- ^ Scrutinee
        -> [(f a, HExp b)]  -- ^ Matches (pattern -> body)
        -> HExp b

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
hmerge :: (Show a, Show b, Partable f a) => HExp a -> [(f a, HExp b)] -> HExp b
hmerge = HMerge

hmatch ::
    forall a b f .
    ( Show a
    , Show b
    -- Scrutinee must be representable as a finite/enumerable type.
    , Partable f a
    )
    => HExp a           -- ^ Scrutinee
    -> (f a -> HExp b)  -- ^ Matching function
    -> HExp b           -- ^ Return an HMerge
hmatch e f = hmerge e branches
  where
    pats :: [f a]
    pats = [minBound ..]

    bodies :: [HExp b]
    bodies = map f pats

    -- NOTE: We use 'next' here, but what would that look like in Haski?
    -- We don't have the "next" value then.
    scrut :: a
    scrut = next e

    branches :: [(f a, HExp b)]
    branches = zip pats bodies

-- Maybe this isn't really possible in Haski, when using actual streams.
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
-- * Patterns
--

-- | Class for types which can be partitioned into a bounded/enumerable type.
class (Bounded (f a), Enum (f a), Show (f a)) => Partable f a where
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


--
-- * Misc
--

-- Test program
tp :: Float -> HExp Int
tp x = hval x `hmatch` inspect
  where
    inspect :: PatSign Float -> HExp Int
    inspect pf = hval $ case pf of
        Pos  -> 1
        Neg  -> -1
        Zero -> 0

tp2 :: String -> String -> HExp Bool
tp2 s1 s2 = hval $ case ( next $ hval s1 `hmatch` inspect
                        , next $ hval s2 `hmatch` inspect
                        ) of
    (True, True) -> True
    _            -> False
  where
    inspect s = hval $ case s of
        Ascii -> True
        Other -> False
