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
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens
import qualified Lens.Micro.TH as Lens

import Data.Proxy (Proxy (..))


--
-- * State/environment
--

-- ** This stuff goes first so that TH doesn't freak out

-- | Keeps track of some stuff we need while constructing the Xp program.
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

--
-- * Main data type
--

data Xp a where
    X :: Xp a

    Val :: (CType a) => a -> Xp a
    Var :: (CType a) => String -> Xp a

    -- | Dangerous?
    Cast ::
        ( Show a
        , CType b
        )
        => TRep b
        -> Xp a
        -> Xp b

    {- | Symbolic variable. These are used to refer to other expressions,
    such as the scrutinee in a case construct. Requires the referee to be
    a variable (so that it can be named).
    -}
    SVar ::
           String  -- ^ The name of the variable that is being referenced.
        -> Xp a

    SFieldRef ::
        ( Struct pt
        , CType a
        )
        => FieldRef pt
        -> Xp a

    -- | Representation of branching on partitioning.
    SumMatch ::
        ( Show a
        , CType a
        , CType b
        )
        => Scrut a
        -- ^ Scrutinee tagged with a name. The scrutinee needs to be tagged
        -- so that we know how to refer to it in the body of the matches.
        -> [(Xp Bool, Xp b)]
        -- ^ Matches, consisting of a predicate, and the body of the match.
        -- The body of the match uses 'SVar's to refer to the scrutinee.
        -> Xp b

    -- | Representation of deconstruction of a product type.
    ProdMatch ::
        ( Show a
        , ToStruct a pt
        , CType a
        , CType b
        )
        => Proxy pt
        -- ^ Type of the struct being deconstructed. Needed so that the type
        -- can be brought into scope during compilation.
        -> Scrut a
        -> Xp b
        -> Xp b

    -- Primitive operators.
    Add :: (CType a, Num a) => Xp a -> Xp a -> Xp a
    Mul :: (CType a, Num a) => Xp a -> Xp a -> Xp a
    Sub :: (CType a, Num a) => Xp a -> Xp a -> Xp a
    Div :: (Fractional a) =>   Xp a -> Xp a -> Xp a
    Gt  :: (Show a, Num a) =>  Xp a -> Xp a -> Xp Bool
    Lt  :: (Show a, Num a) =>  Xp a -> Xp a -> Xp Bool
    Eq  :: (Show a, Eq a) =>   Xp a -> Xp a -> Xp Bool
    Not ::                     Xp Bool -> Xp Bool
    And ::                     Xp Bool -> Xp Bool -> Xp Bool
    Or  ::                     Xp Bool -> Xp Bool -> Xp Bool
deriving stock instance Show a => Show (Xp a)

-- | Data type representing the scrutinee in a case construct.
data Scrut a = Scrut
    String
    -- ^ Scrutinee name. Used by other constructs to refer to the scrutinee.
    (Xp a)
    -- ^ Value of the scrutinee.
    deriving Show

-- ** Typing related stuff

class BaseType a
instance BaseType Int
instance BaseType Double
instance BaseType Bool

-- | Allow only reference types of allowed base types.
data BaseType a => RefType a

-- | Translation from Haskell types to C.
class CType a where
    cType :: String

instance CType Int where
    cType = "int"

instance CType Double where
    cType = "double"

instance CType Bool where
    cType = "bool"

instance CType a => CType (RefType a) where
    cType = cType @a ++ "*"

instance CType Char where
    -- TODO: This is a placeholder, probably want to look over this instance
    -- and any examples that need it.
    cType = "char"

-- ** Convenient instances

instance (CType a, Num a) => Num (Xp a) where
    (+) = Add
    (*) = Mul
    (-) = Sub
    fromInteger n = Val (fromInteger n)
    abs = error "not implemented"
    signum = error "not implemented"

instance (CType a, Fractional a) => Fractional (Xp a) where
    fromRational xrat = Val (fromRational xrat)
    (/) = Div

--
-- * Struct type representation and pattern matching
--

{- | Instances of @ToStruct a pt@ define translations from type @a@ to struct
type @b@.
-}
class Struct pt => ToStruct a pt where
    toStruct :: Xp a -> pt

-- | Class for types that can represent a product/struct type.
class Struct pt where
    -- | Struct types must define their name.
    structName :: String

    -- | Struct types must define how they convert to a list of 'Field's.
    toFields :: pt -> Fields pt

    -- | Struct types must define how they convert from a list of 'Field's.
    fromFields :: Fields pt -> pt

    {- | Struct types must define a dummy value, used only for its structure.
    The values of its 'Field's can be undefined. The dummy values should not
    be used.
    -}
    dummy :: pt  -- Generate this with GHC generics? Or maybe TH? This is ugly.

type Fields a = [Field]
data Field = forall a . (CType a, Show a) => Field
    (TRep a)  -- ^ Type of field as a constructor.
    String    -- ^ Name of field.
    (Xp a)    -- ^ Value of field.
deriving instance Show Field

data TRep a where
    TBool   :: TRep Bool
    TInt    :: TRep Int
    TDouble :: TRep Double
deriving instance Show a => Show (TRep a)

showTRep :: TRep a -> String
showTRep TBool = "bool"
showTRep TInt = "int"
showTRep TDouble = "double"

data FieldRef a = FieldRef
    String  -- ^ Name of the variable being referenced.
    Int     -- ^ Index of struct, i.e. which field of the struct.
    deriving Show

-- ** Combinators

-- | Deconstruction on a scrutinee that can be translated to a struct type.
deconstruct :: forall a p b .
    ( ToStruct a p
    , Show a
    , CType a
    , CType b
    )
    => Xp a
    -> (p -> Xp b)
    -> Hiska (Xp b)
deconstruct scrut f = do
    -- Generate an internal name for the scrutinee (see why below).
    scrutId <- freshId

    let body = f (symStruct scrutId)

    -- The reason for using Proxy is to give the compiler information about
    -- the struct type, so that it can generate the proper names and types.
    pure $ ProdMatch (Proxy @p) (Scrut scrutId scrut) body
  where
    -- Create an instance of the struct type with all fields as symbolic
    -- references (SFieldRef) pointing to the scrutinee. We need to do this
    -- so that the variables referenced by the _user_ actually refer to the
    -- scrutinee, since we don't actually transform the scrutinee (instead,
    -- we use its type to generate a _new_ struct). Because the user will
    -- be using variables from this generated struct and not the actual
    -- scrutinee, we need to manually maintain the relationship between the
    -- generated struct and the scrutinee.
    symStruct :: String -> p
    symStruct scrutId =
        fromFields
        $ zipWith mkSymField [0 ..]
        $ toFields (dummy @p)
      where
        mkSymField :: Int -> Field -> Field
        mkSymField idx (Field t s _) =
            Field t s $ SFieldRef @p $ FieldRef scrutId idx

-- | 'deconstruct' with arguments flipped.
as :: forall a s b .
    ( ToStruct a s
    , Show a
    , CType a
    , CType b
    )
    => (s -> Xp b)
    -> Xp a
    -> Hiska (Xp b)
as = flip deconstruct

--
-- * Partitioning
--

{-| Instances of @Partition a p@ define how values of type @a@ can be
partitioned into type @p@. @p@ must be enumerable; its constructors represent
a finite number of partitions for a possibly non-finite type @a@.
-}
class Enum p => Partition a p where
    {-| @Partition a p@ instances must define how an arbitrary value of type
    @a@ is partitioned. In other words, they must describe
    (in the Xp language) rules for how to translate from @a@ to @p@.
    -}
    partition :: Xp a -> [(p, Xp Bool)]

branch :: forall a p s b .
    ( Partition a p
    , Show a
    , CType a
    , CType b
    )
    => Xp a
    -> (p -> Xp a -> Hiska (Xp b))
    -> Hiska (Xp b)
branch scrut f = do
    -- We need to generate an internal name for the scrutinee in order to
    -- make explicit the connection between the variable the _user_ references,
    -- and the actual scrutinee.
    scrutId <- freshId

    let (parts, preds) = unzip $ partition @a @p (SVar scrutId)
    bodies <- mapM (\ p -> f p (SVar scrutId)) parts

    pure $ SumMatch (Scrut scrutId scrut) (zip preds bodies)

--
-- * Other combinators
--

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

xvar :: CType a => String -> Xp a
xvar = Var

xval :: CType a => a -> Xp a
xval = Val

cast :: (CType a, CType b, Show a) => TRep b -> Xp a -> Xp b
cast = Cast
