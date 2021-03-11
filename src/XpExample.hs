{- | Examples using Xp.

Print the C output of a program @(p :: Xp a)@ with

> printProg p
-}

module XpExample where

import Xp
import XpCompile (compile)


printProg :: Show a => Xp a -> IO ()
printProg = putStrLn . compile

{- | Increment by 1 for positive values, return 0 otherwise.

@
GHCi> printProg $ xprog1 (xvar "scrut")
{
int scrut = scrut;
int result;
if (scrut > 0) { result = scrut + 1 }
if (scrut < 0) { result = 0 }
}
@
-}
xprog1 :: Xp Int -> Xp Int
xprog1 var = xcase var conds bodies
  where
    conds :: Xp Int -> [Xp Bool]
    conds sv = [sv >. 0, sv <. 0]

    bodies :: Int -> Xp Int -> Xp Int
    bodies = \case
        0 -> pos
        1 -> neg

    pos :: Xp Int -> Xp Int
    pos = (+ 1)

    neg :: Xp Int -> Xp Int
    neg = const 0

{- | Return a string depending on the value of the scrutinee.

@
GHCi> printProg xprog2
{
int scrut = scrutinee - 12;
int result;
if (scrut > 0) { result = "The number is Positive!" }
if (scrut < 0) { result = "The number is Negative!" }
if (scrut == 0) { result = "The number is Zero!" }
}
@
-}
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

-- | Simple nested case expressions.
xprog3 :: Xp Int
xprog3 = 3 + xprog1 (xprog1 2)

--
-- * Product type examples
--

data V = V
    { vx :: Int
    , vy :: Int
    } deriving (Eq, Show)

instance ProdType V where
    absArgs = [AbsField TInt "vx", AbsField TInt "vy"]
    args (V x y) = [Field TInt "vx" (xval x), Field TInt "vy" (xval x)]
    consName = "V"

xprog4 :: Xp Int
xprog4 = xcasep @V (xval (V 5 9)) $ \case
    [Field TInt "vx" x, Field TInt "vy" y] -> x + y

