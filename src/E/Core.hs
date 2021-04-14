{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module E.Core where

import Data.Kind
import Data.Void

import Data.SOP
import Generics.SOP

import qualified Control.Monad.State.Strict as St
import qualified GHC.Generics as GG


data Sig = Pos (E Int) | Neg deriving GG.Generic
instance Generic Sig

class Generic p => Partition p a where
    type P p :: [[*]]

    partition :: [E a -> (E Bool, SOP I (P p))]

instance Partition Sig Double where
    type P Sig = '[ '[E Int] , '[] ]

    partition =
        [ \ v -> (v >. 0, from $ Pos $ c_floorInt v)
        , \ v -> (v >. 0, from Neg)
        ]

case' :: forall p a b . Partition p a
    => E a
    -> (SOP I (P p) -> E b)
    -> Estate (E b)
case' s f = do
    scrutVar <- freshId

    let branches = map ($ ESym scrutVar) $ partition @p @a
    let matches = map (\ (cond, sop) -> Match @p @b cond sop (f sop)) branches

    pure $ ECase (Scrut s scrutVar) matches

ex :: E Double -> Estate (E Int)
ex v = case' @Sig v $ \case
    SOP (Z (I n :* Nil)) -> n + 1
    _ -> undefined
    -- Neg   -> 0

data Scrut a = Scrut (E a) String

data Match p b where
    Match
        :: E Bool
        -> SOP I (P p)
        -> E b
        -> Match p b

data E a where
    EVal :: a -> E a
    EVar :: String -> E a

    ESym :: String -> E a
    ECase :: Partition p a => Scrut a -> [Match p b] -> E b

    EAdd :: (Num a) =>        E a -> E a -> E a
    EMul :: (Num a) =>        E a -> E a -> E a
    ESub :: (Num a) =>        E a -> E a -> E a
    EDiv :: (Fractional a) => E a -> E a -> E a
    EGt  :: (Num a) =>        E a -> E a -> E Bool
    ELt  :: (Num a) =>        E a -> E a -> E Bool
    EEq  :: (Eq a) =>         E a -> E a -> E Bool
    ENot ::                   E Bool -> E Bool
    EAnd ::                   E Bool -> E Bool -> E Bool
    EOr  ::                   E Bool -> E Bool -> E Bool

    -- C stuff

    ECFloorInt    :: E Double -> E Int
    ECFloorDouble :: E Double -> E Double

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

infix 4 >.
(>.) :: (Num a) => E a -> E a -> E Bool
(>.) = EGt

infix 4 <.
(<.) :: (Num a) => E a -> E a -> E Bool
(<.) = ELt

infix 4 ==.
(==.) :: (Eq a) => E a -> E a -> E Bool
(==.) = EEq

infix 4 /=.
(/=.) :: (Eq a) => E a -> E a -> E Bool
x /=. y = e_not (x `EEq` y)

infixr 3 &&.
(&&.) :: E Bool -> E Bool -> E Bool
(&&.) = EAnd

infixr 2 ||.
(||.) :: E Bool -> E Bool -> E Bool
(||.) = EOr

e_not :: E Bool -> E Bool
e_not = ENot

e_var :: String -> E a
e_var = EVar

e_val :: a -> E a
e_val = EVal


-- * Uses <math.h>

c_floorDouble :: E Double -> E Double
c_floorDouble = ECFloorDouble

c_floorInt :: E Double -> E Int
c_floorInt = ECFloorInt

c_fracPart :: E Double -> E Double
c_fracPart d = d - c_floorDouble d


-- * Auxiliary stuff

type Estate = St.State Int

runEstate :: Estate a -> a
runEstate x = St.evalState x 0

-- | Return a unique identifier and increment the counter in state.
freshId :: Estate String
freshId = do
    newId <- ("coreId" ++) . show <$> St.get
    St.modify'(+ 1)
    pure newId
