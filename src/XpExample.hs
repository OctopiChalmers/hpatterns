module XpExample where

import Xp


printProg :: Show a => Xp a -> IO ()
printProg = pPrintXp

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

xprog2 :: Xp String
xprog2 = xcase scrut conds bodies
  where
    scrut :: Xp Int
    scrut = xvar "scrutinee" - 12

    conds :: Xp Int -> [Xp Bool]
    conds sv = [sv >. 0, sv <. 0, sv ==. 0]

    bodies :: Int -> Xp Int -> Xp String
    bodies = \case
        0 -> const $ xval "The number is Positive!"
        1 -> const $ xval "The number is Negative!"
        2 -> const $ xval "The number is Zero!"
