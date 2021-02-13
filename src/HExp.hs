{-# LANGUAGE GADTs                 #-}
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

    HMerge :: (Show a, Show b) =>
        HExp a -> [(a, HExp b)] -> HExp b

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
hmerge :: (Show a, Show b) => HExp a -> [(a, HExp b)] -> HExp b
hmerge = HMerge

hmatch ::
    forall a b .
    ( Show a
    , Show b
    )
    => HExp a        -- ^ Scrutinee
    -> (Pat a -> b)  -- ^ Matching function
    -> HExp b        -- ^ Return an HMerge
hmatch e f = hmerge e branches
  where
    scrut :: a
    scrut = next e

    -- branches :: [(Pat a, HExp b)]
    branches = undefined

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
data Num a => Pat a = Pos | Neg
    deriving (Bounded, Enum)

-- | Class for types which can be partitioned into a bounded/enumerable type.
class Partable a f where
    part :: (Bounded (f a), Enum (f a)) => a -> f a

instance Partable Float Pat where
    part :: Float -> Pat Float
    part x
        | x >= 0    = Pos  -- 0 is treated as positive for now
        | otherwise = Neg
