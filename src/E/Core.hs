{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module E.Core where


data E a where
    EVal :: a -> E a
    EVar :: String -> E a

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
x /=. y = enot (x `EEq` y)

infixr 3 &&.
(&&.) :: E Bool -> E Bool -> E Bool
(&&.) = EAnd

infixr 2 ||.
(||.) :: E Bool -> E Bool -> E Bool
(||.) = EOr

enot :: E Bool -> E Bool
enot = ENot

evar :: String -> E a
evar = EVar

eval :: a -> E a
eval = EVal
