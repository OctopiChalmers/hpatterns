module Xp where

data Xp a where
    Val :: a -> Xp a
    SymVar :: Xp a

    Add :: Num a => Xp a -> Xp a -> Xp a
    Gt :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
    And :: Xp Bool -> Xp Bool -> Xp Bool
    Eq :: (Show a, Eq a) => Xp a -> Xp a -> Xp Bool
deriving stock instance Show a => Show (Xp a)

instance Num a => Num (Xp a) where
    fromInteger e = Val $ fromInteger e
    e1 + e2 = Add e1 e2

xcase :: forall a b .
    ( Show a
    )
    => (Xp a -> [Xp Bool])
    -> (Int -> Xp a -> Xp b)
    -> Xp b
xcase = undefined
