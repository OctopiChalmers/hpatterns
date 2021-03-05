{- | Code from Young 2021. "On Adding Pattern Matching to Haskell-Based Deeply
Embedded Domain Specific Languages".
-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Case3 () where  -- This module is only used as a reference for now.

import Data.Void
import Data.Typeable


data E t where
    ConstructRep :: (Typeable a, ERep a) => E (ERepTy a) -> E a
    ELeft  :: E a -> E (Either a b)
    ERight :: E b -> E (Either a b)
    EPair  :: E a -> E b -> E (a, b)

    ECase ::
        ( ERep t
        , ERepTy (ERepTy t) ~ ERepTy t
        )
        => E t
        -> E (SumMatch (ERepTy t) r)
        -> E r

    ESumMatch ::
        ( ERep a
        , ERep b
        , ERepTy b ~ b
        )
        => E (ProdMatch a r)
        -> E (SumMatch b r)
        -> E (SumMatch (Either a b) r)

    EOneSumMatch ::
        ( ERep a
        , ERep b
        , ERepTy a ~ a
        )
        => E (ProdMatch a b)
        -> E (SumMatch a b)

    EEmptyMatch :: (ERep b) => E (SumMatch Void b)

    ProdMatchExp ::
        ( ERep a
        , ERep b
        )
        => E (a -> ProdMatch b r)
        -> E (ProdMatch (a, b) r)

    NullaryMatch :: (ERep a) => E r -> E (ProdMatch a r)

newtype ProdMatch a b = ProdMatch
    { unProdMatch :: a -> b
    }

newtype SumMatch a b = SumMatch
    { unSumMatch :: a -> b
    }

class ERep t where
    type ERepTy t
    construct :: t -> E (ERepTy t)
    rep :: t -> E t
    -- default
    --     rep :: (ERep (ERepTy t)) => t -> E t
    -- rep x = ConstructRep (construct x)
    unrep' :: ERepTy t -> t
    rep' :: t -> ERepTy t

instance (ERep a, ERep b) => ERep (Either a b) where
    type ERepTy (Either a b) = Either a b
    construct (Left x) = ELeft (rep x)
    construct (Right y) = ERight (rep y)
    rep (Left x)  = ELeft (rep x)
    rep (Right y) = ERight (rep y)
    unrep' = id
    rep'   = id

instance (ERep a, ERep b) => ERep (a, b) where
    type ERepTy (a, b) = (a, b)
    construct (x, y) = EPair (rep x) (rep y)
    rep (x, y) = EPair (rep x) (rep y)
    unrep' = id
    rep'   = id
