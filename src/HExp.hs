{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module HExp where

-- | Main data type.
data HExp a where
    HVal   ::
        a -> HExp a

    HFby   ::
        a -> HExp a -> HExp a

    HWhen  :: (Show a, Show b) =>
        HExp a -> (HExp b, b) -> HExp a

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

hwhen :: HExp a -> (HExp b, b) -> HExp a
hwhen = undefined

{- | Representation of a case-of expression. Scrutinee is of type 'a', return
value is of type 'b'.
-}
hmerge
    :: HExp a
    -> [(a, HExp b)]
    -> HExp b
hmerge = undefined
