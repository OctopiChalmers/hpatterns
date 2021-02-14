{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

module HExp where


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

    HMerge :: (Show a, Show b, Num a)
        => HExp a             -- ^ Scrutinee
        -> [(Pat a, HExp b)]  -- ^ Matches (pattern -> body)
        -> HExp b

deriving instance Show a => Show (HExp a)


-- * Combinators

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
hmerge :: (Show a, Show b, Num a) => HExp a -> [(Pat a, HExp b)] -> HExp b
hmerge = HMerge

hmatch ::
    forall a b .
    ( Show a
    , Show b
    -- Scrutinee must be representable as a finite/enumerable type.
    , Partable a Pat
    , Num a  -- Temporary constraint
    )
    => HExp a        -- ^ Scrutinee
    -> (Pat a -> HExp b)  -- ^ Matching function
    -> HExp b        -- ^ Return an HMerge
hmatch e f = hmerge e branches
  where
    pats :: [Pat a]
    pats = [minBound ..]

    bodies :: [HExp b]
    bodies = map f pats

    scrut :: a
    scrut = next e

    branches :: [(Pat a, HExp b)]
    branches = zip pats bodies

-- Maybe this isn't really possible in Haski, with actual streams.
next :: HExp a -> a
next = \case
    HFby x e -> x
    HVal x   -> x

    HWhen e1 (e2, x) def
        | next e2 == x -> next e1
        | otherwise -> next def

    HMerge e branches -> undefined

-- What does "Num a" do here? Does it do anything?
-- Consider rewriting as an GADT?
data Num a => Pat a = Pos | Zero | Neg
    deriving (Bounded, Enum, Show)

-- | Class for types which can be partitioned into a bounded/enumerable type.
class Partable a f where
    part :: (Bounded (f a), Enum (f a)) => a -> f a

instance Partable Float Pat where
    part :: Float -> Pat Float
    part x
        | x > 0     = Pos
        | x < 0     = Neg
        | otherwise = Zero

-- Test program
tp :: Float -> HExp String
tp x = hval x `hmatch` inspect
  where
    inspect :: Pat Float -> HExp String
    inspect pf = hval $ case pf of
        Pos  -> "Positive!"
        Neg  -> "Negative!"
        Zero -> "Zero!"

