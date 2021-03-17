{- | Core module of the expression language.

Defines the data type, necessary classes, and key combinators.
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TemplateHaskell       #-}

module Xp.Core where

import qualified Control.Monad.State.Strict as St
import qualified Data.IntSet as IS
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens
import qualified Lens.Micro.TH as Lens


-- | Main data type.
data Xp a where
    Val :: a -> Xp a
    Var :: String -> Xp a
    SVar :: String -> Xp a

    Case :: (Show a)
        => (String, Xp a)     -- ^ Scrutinee tagged with a name.
        -> [(Xp Bool, Xp b)]  -- ^ Matches (condition -> body)
        -> Xp b

    Add :: (Num a) =>         Xp a -> Xp a -> Xp a
    Mul :: (Num a) =>         Xp a -> Xp a -> Xp a
    Sub :: (Num a) =>         Xp a -> Xp a -> Xp a
    Div :: (Fractional a) =>  Xp a -> Xp a -> Xp a

    Gt  :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
    Lt  :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
    Eq  :: (Show a, Eq a) =>  Xp a -> Xp a -> Xp Bool
    Not ::                    Xp Bool -> Xp Bool
    And ::                    Xp Bool -> Xp Bool -> Xp Bool
    Or  ::                    Xp Bool -> Xp Bool -> Xp Bool
deriving stock instance Show a => Show (Xp a)

instance Num a => Num (Xp a) where
    (+) = Add
    (*) = Mul
    (-) = Sub
    fromInteger n = Val (fromInteger n)
    abs = error "not implemented"
    signum = error "not implemented"

instance Fractional a => Fractional (Xp a) where
    fromRational xrat = Val (fromRational xrat)
    (/) = Div

--
-- * State/environment
--

-- | Keeps track of some stuff we need when constructing the Xp program.
data Hst = Hst
    { _hstCounter :: Int
    }
$(Lens.makeLenses ''Hst)

type Hiska = St.State Hst

runHiska :: Hiska a -> a
runHiska x = St.evalState x initHst
  where
    initHst :: Hst
    initHst = Hst 0

-- | Return a unique identifier and increment the counter in state.
freshId :: Hiska String
freshId = do
    newId <- ("coreId" ++) . show <$> Lens.use hstCounter
    Lens.modifying hstCounter (+ 1)
    pure newId

freshTag :: Hiska (Xp a)
freshTag = SVar <$> freshId

--
-- * Partitioning
--

{- | Data type containing necessary stuff to build case constructions.

The lists need be of equal length, or behavior is undefined. I.e. for any

> PartitionData xs ys

The following must hold:

> length xs == ys length
-}
data PartitionData p a = PartitionData
    [Xp Bool]  -- Predicates for seclecting a branch.
    [p a]      -- Fully applied constructors for the type (p a)

-- Vectors would check this with types instead.
partitionIsAligned :: forall p a . Partition p a => Bool
partitionIsAligned =
    let PartitionData preds constructors = partition @p @a (error "DUMMY")
    in 1 == IS.size (IS.fromList [length preds, length constructors])

class Partition (p :: * -> *) a where
    partition :: Xp a -> PartitionData p a

--
-- * Combinators
--

case' :: forall p a b .
    ( Partition p a
    , Show a
    )
    => Xp a
    -> (p a -> Hiska (Xp b))
    -> Hiska (Xp b)
case' scrut f = do
    -- Generate new tag to keep track of which scrutinee we refer to
    -- in the body of a match.
    scrutName <- freshId

    -- Generate the predicates and applied constructors for the input parition
    -- type.
    let PartitionData preds constructors = partition @p @a (SVar scrutName)

    -- Generate the bodies of the matches by applying the input function
    -- to every possible constructor; compare to The Trick.
    bodies <- mapM f constructors

    pure $ Case (scrutName, scrut) (zip preds bodies)

infix 4 >.
(>.) :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
(>.) = Gt

infix 4 <.
(<.) :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
(<.) = Lt

infix 4 ==.
(==.) :: (Show a, Eq a) => Xp a -> Xp a -> Xp Bool
(==.) = Eq

infix 4 /=.
(/=.) :: (Show a, Eq a) => Xp a -> Xp a -> Xp Bool
x /=. y = xnot (x `Eq` y)

infixr 3 &&.
(&&.) :: Xp Bool -> Xp Bool -> Xp Bool
(&&.) = And

infixr 2 ||.
(||.) :: Xp Bool -> Xp Bool -> Xp Bool
(||.) = Or

xnot :: Xp Bool -> Xp Bool
xnot = Not

xvar :: String -> Xp a
xvar = Var

xval :: a -> Xp a
xval = Val
