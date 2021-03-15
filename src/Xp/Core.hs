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

module Xp.Core where


-- | Main data type.
data Xp a where
    Val :: a -> Xp a
    Var :: String -> Xp a
    SVar :: Xp a

    Case :: (Show a)
        => Xp a               -- ^ Scrutinee
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
-- * Partitioning
--

class Partition (p :: * -> *) a where
    {- | Predicates for determining how to branch to the possible partitions.

    The ordering of the predicates in the output list matters, since they
    will each be zipped with a constructor of the partition type (in order).

    For example, from the following definitions, the (> 0) condition will
    be used for the @Pos@ constructor, and (< 0) for the @Neg@ constructor:

    @
    data Sig a = Pos | Neg deriving (Show, Enum, Bounded)
    instance Partition Sig Int
        conds var = [var >. 0, var <. 0]
    @

    TODO: Make this independent of order (maybe using tuples)?
    -}
    conds :: Xp a -> [Xp Bool]

    {- | TODO: Generate this automatically and in a generic way.

    After all, we know that all constructors of (p a) are going
    to have SVar as their arguments.

    For now, 'Xp.TH.makeConstructors' automates this a little bit, but
    is not ideal.
    -}
    constructors :: [p a]

--
-- * Combinators
--

case' :: forall p a b .
    ( Partition p a
    , Show a
    )
    => Xp a
    -> (p a -> Xp b)
    -> Xp b
case' scrut f = Case scrut (zip preds bodies)
  where
    preds :: [Xp Bool]
    preds = conds @p @a SVar

    bodies :: [Xp b]
    bodies = map f (constructors @p @a)

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
