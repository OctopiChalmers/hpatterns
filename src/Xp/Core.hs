{- | Core module of the expression language.

Defines the data type, necessary classes, and key combinators.
-}

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Xp.Core where


-- | Main data type.
data Xp a where
    Val :: a -> Xp a
    Var :: String -> Xp a
    SVar :: Xp a
    SField :: String -> Xp a
        -- ^ Like an SVar but with a name to refer to a field of a struct.

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
-- * Combinators
--

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
