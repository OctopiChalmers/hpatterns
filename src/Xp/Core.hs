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
{-# LANGUAGE TemplateHaskell       #-}
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

    -- TODO: Any way to do this automatically, in a generic way? After all, we
    -- know that all constructors of (p a) are going to have SVar as their
    -- arguments.
    constructors :: [p a]

-- Data type declarations can't be in the same module as splice :(
data Num a => Sig a
    = Pos (Xp a)
    | Neg (Xp a)
    deriving (Show)

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

    parts :: [p a]
    parts = constructors

    bodies :: [Xp b]
    bodies = map f parts

-- | Build a case expression.
xcase :: forall a b .
    ( Show a
    )
    => Xp a
    -- ^ This will be the scrutinee.
    -> (Xp a -> [Xp Bool])
    -- ^ Function which, given a symbolic variable, returns a predicate for
    -- each branch of the case-expression.
    -> (Int -> Xp a -> Xp b)
    -- ^ Function which, given an index, returns a function which can be
    -- applied to the scrutinee to produce the body of a branch in the
    -- case expression. The index indicates which of the predicates to
    -- match against.
    -> Xp b
xcase var condFun bodyFun = Case var (zip conds bodies)
  where
    conds :: [Xp Bool]
    conds = condFun SVar

    bodies :: [Xp b]
    bodies = trick (length conds) bodyFun

    trick ::
           Int
        -> (Int -> Xp a -> Xp b)
        -> [Xp b]
    trick n f = map ($ SVar) fs
      where
        fs :: [Xp a -> Xp b]
        fs = map f [0 .. n]

(>.) :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
(>.) = Gt

(<.) :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
(<.) = Lt

(==.) :: (Show a, Eq a) => Xp a -> Xp a -> Xp Bool
(==.) = Eq

(&&.) :: Xp Bool -> Xp Bool -> Xp Bool
(&&.) = And

(||.) :: Xp Bool -> Xp Bool -> Xp Bool
(||.) = Or

xnot :: Xp Bool -> Xp Bool
xnot = Not

xvar :: String -> Xp a
xvar = Var

xval :: a -> Xp a
xval = Val
