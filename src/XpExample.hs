module XpExample where

import Xp


-- | Increment by 1 for positive values, return 0 otherwise.
xprog1 :: Xp Int -> Xp Int
xprog1 var = xcase var f g
  where
    f :: Xp Int -> [Xp Bool]
    f sv =
        [ sv >. 0
        , 0 >. sv
        ]

    g :: Int -> Xp Int -> Xp Int
    g n = case n of
        0 -> pos
        1 -> neg

    pos :: Xp Int -> Xp Int
    pos sv = sv + 1

    neg :: Xp Int -> Xp Int
    neg _ = 0
