module Xp where

data Xp a where
    Val :: a -> Xp a
    SymVar :: Xp a

    Case :: [(Xp Bool, Xp a)] -> Xp a

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
xcase g f = Case (zip conds bodies)
  where
    conds :: [Xp Bool]
    conds = g SymVar

    bodies :: [Xp b]
    bodies = aTrick (length conds) f

aTrick :: forall a b .
       Int
    -> (Int -> Xp a -> Xp b)
    -> [Xp b]
aTrick n f = map ($ SymVar) bodies
  where
    bodies :: [Xp a -> Xp b]
    bodies = map f [0 .. n]


-- | Increment by 1 for positive values, return 0 otherwise.
xprog1 :: Xp Int
xprog1 = xcase f g
  where
    f :: Xp Int -> [Xp Bool]
    f sv =
        [ sv `Gt` Val 0  -- x > 0
        , Val 0 `Gt` sv  -- 0 > x
        ]

    g :: Int -> Xp Int -> Xp Int
    g n = case n of
        0 -> pos
        1 -> neg
        _ -> error "boom"

    pos :: Xp Int -> Xp Int
    pos sv = sv `Add` Val 1

    neg :: Xp Int -> Xp Int
    neg _ = Val 0
