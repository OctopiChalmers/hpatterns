module XpExample where

import Xp


-- | Increment by 1 for positive values, return 0 otherwise.
xprog1 :: Xp Int
xprog1 = xcase (Var "e") f g
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
