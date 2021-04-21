{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Module defining the deep embedding of the expression language 'E'. Also
includes functions and types related to the construction of the 'E' AST, such
as keeping track of internal state and such.
-}

module E.Core where

import Data.Bits (Bits)
import Data.Proxy (Proxy(..))

import E.CTypes (CType)

import qualified Control.Monad.State.Strict as St


--
-- * Main data type
--

data E a where
    -- Constructors for pattern matching.
    ESym :: ScrutId -> E a
    ECase :: (CType a, CType b)
        => Scrut a
        -> [Match p b]
        -> E b

    EField :: ArgId -> E a -> E a

    -- Straightforward operators.

    EVal :: a -> E a
    EVar :: String -> E a

    EAdd :: (Num a) =>          E a -> E a -> E a
    EMul :: (Num a) =>          E a -> E a -> E a
    ESub :: (Num a) =>          E a -> E a -> E a
    EDiv :: (Fractional a) =>   E a -> E a -> E a
    EGt  :: (Num a, CType a) => E a -> E a -> E Bool
    ELt  :: (Num a, CType a) => E a -> E a -> E Bool
    EGte :: (Num a, CType a) => E a -> E a -> E Bool
    ELte :: (Num a, CType a) => E a -> E a -> E Bool
    EEq  :: (Eq a, CType a) =>  E a -> E a -> E Bool
    ENot ::                     E Bool -> E Bool
    EAnd ::                     E Bool -> E Bool -> E Bool
    EOr  ::                     E Bool -> E Bool -> E Bool

    ECFloorInt    :: E Double -> E Int
    ECFloorDouble :: E Double -> E Double

    -- UNSAFE bit twiddling stuff. This business should be handled using some
    -- safer abstraction in the future; the capability is here to enable
    -- some examples.

    ECast   :: (CType a, CType b) => E a -> Proxy b -> E b  -- yikes
    EShiftL :: (Bits a) => E a -> E Int -> E a
    EShiftR :: (Bits a) => E a -> E Int -> E a
    EBitAnd :: (Bits a) => E a -> E a -> E a

data Scrut a = Scrut (E a) ScrutId

{- | A @Match p b@ represents one branch in a pattern match, i.e. one
constructor of a sum type @p@. @p@ is the type pattern matched on, and @b@
is the return type of the body, i.e. the right hand side of a case-of arrow.
-}
data Match p b where
    Match :: forall p b . CType b
        => E Bool     -- ^ Predicate for selecting this particular branch
        -> E b        -- ^ Body of branch; what is returned if chosen.
        -> Match p b

-- Helpful instances for 'E' for better ergonomics.

instance (Num a) => Num (E a) where
    (+) = EAdd
    (*) = EMul
    (-) = ESub
    fromInteger n = EVal (fromInteger n)
    abs = error "not implemented"
    signum = error "not implemented"

instance (Fractional a) => Fractional (E a) where
    fromRational r = EVal (fromRational r)
    (/) = EDiv


--
-- * AST construction monad internals.
--

newtype ScrutId = ScrutId Int deriving (Eq, Ord, Show)
newtype FieldId = FieldId Int deriving (Eq, Ord, Show)

{- | An 'ArgId' is an identifier for a single bound field of a deconstructed
value in a pattern match. The idea is to attach 'ArgId's as tags to the
expressions representing the constructor fields for partition types. See
'newFieldTag'.
-}
data ArgId = ArgId ScrutId FieldId
    deriving (Eq, Ord, Show)

{- | Computation state during construction of the program AST. Used to generate
unique identifiers as they are needed.
-}
type Estate = St.State Env

data Env = Env
    { envScrutCount :: Int
    , envFieldCount :: Int
    }

-- | Run a computation, constructing an AST (probably).
runEstate :: Estate a -> a
runEstate x = St.evalState x initEnv
  where
    initEnv :: Env
    initEnv = Env 0 0

-- | Return a unique identifier to name the pattern matched scrutinees.
newScrutId :: Estate ScrutId
newScrutId = do
    St.modify' (\ st -> st { envScrutCount = envScrutCount st + 1 })
    ScrutId . envScrutCount <$> St.get

{- | Return a computation for tagging an expression with an identifier.
This is used on constructor fields to reduce redundant computation in
the generated C code.
-}
newFieldTag :: Estate (E a -> E a)
newFieldTag = do
    St.modify' (\ st -> st { envFieldCount = envFieldCount st + 1 })
    scrutId <- ScrutId . envScrutCount <$> St.get
    fieldId <- FieldId . envFieldCount <$> St.get
    pure $ EField (ArgId scrutId fieldId)
