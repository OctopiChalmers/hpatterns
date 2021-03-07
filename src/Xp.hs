module Xp where

import qualified Control.Monad.Trans.Reader as R


-- | Main data type.
data Xp a where
    Val :: a -> Xp a
    Var :: String -> Xp a
    SymVar :: Xp a

    Case :: (Show a)
        => Xp a               -- ^ Scrutinee
        -> [(Xp Bool, Xp b)]  -- ^ Matches (condition -> body)
        -> Xp b

    Add :: (Num a) =>         Xp a -> Xp a -> Xp a
    Gt  :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
    Eq  :: (Show a, Eq a) =>  Xp a -> Xp a -> Xp Bool
    And ::                    Xp Bool -> Xp Bool -> Xp Bool
deriving stock instance Show a => Show (Xp a)

instance Num a => Num (Xp a) where
    fromInteger n = Val (fromInteger n)
    e1 + e2 = Add e1 e2

--
-- * Combinators
--

-- | Build a case expression.
xcase :: forall a b .
    ( Show a
    )
    => Xp a
    -> (Xp a -> [Xp Bool])
    -> (Int -> Xp a -> Xp b)
    -> Xp b
xcase var g f = Case var (zip conds bodies)
  where
    conds :: [Xp Bool]
    conds = g SymVar

    bodies :: [Xp b]
    bodies = aTrick (length conds) f

    aTrick ::
           Int
        -> (Int -> Xp a -> Xp b)
        -> [Xp b]
    aTrick n f = map ($ SymVar) bodies
      where
        bodies :: [Xp a -> Xp b]
        bodies = map f [0 .. n]

-- | @>@ for Xp.
(>.) :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
(>.) = Gt

-- | @==@ for Xp.
(==.) :: (Show a, Eq a) => Xp a -> Xp a -> Xp Bool
(==.) = Eq

--
-- * Prettyprinting
--

pPrintXp :: Show a => Xp a -> IO ()
pPrintXp e = putStrLn $ R.runReader (prettyXp e) Nothing

prettyXp :: forall a .
    ( Show a
    )
    => Xp a
    -> R.Reader (Maybe String) String  -- Env stores the name of the scrutinee
prettyXp (Var s) = pure s
prettyXp (Val v) = pure $ show v
prettyXp SymVar = R.ask >>= \case
    Nothing -> error "unknown SymVar reference"
    Just s -> pure s
prettyXp (Case scrut matches) = do
    let newVar = "x"  -- hardcoded
    scrutStr <- prettyXp scrut
    matchesStrs <- R.local (const $ Just newVar) (mapM prettyXpMatch matches)
    pure $ "int " <> newVar <> " = " <> scrutStr <> ";\n"
        <> unlines matchesStrs <> "\n"
  where
    prettyXpMatch :: (Xp Bool, Xp a) -> R.Reader (Maybe String) String
    prettyXpMatch (cond, body) = do
        condStr <- prettyXp cond
        bodyStr <- prettyXp body
        pure $ "if (" <> condStr <> ") { " <> bodyStr <> " }"
prettyXp (Add e1 e2) = do
    e1Str <- prettyXp e1
    e2Str <- prettyXp e2
    pure $ e1Str <> " + " <> e2Str
prettyXp (Gt e1 e2) = do
    e1Str <- prettyXp e1
    e2Str <- prettyXp e2
    pure $ e1Str <> " > " <> e2Str
