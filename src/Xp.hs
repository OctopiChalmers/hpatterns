module Xp where

import qualified Control.Monad.Trans.Reader as R


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
    fromInteger e = Val $ fromInteger e
    e1 + e2 = Add e1 e2

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

printC :: Show a => Xp a -> IO ()
printC e = putStrLn $ R.runReader (toC e) Nothing

toC :: forall a .
    ( Show a
    )
    => Xp a
    -> R.Reader (Maybe String) String  -- Env stores the name of the scrutinee
toC (Case scrut matches) = do
    let newVar = "x"  -- hardcoded
    scrutStr <- toC scrut
    matchesStrs <- R.local (const $ Just newVar) (mapM toCmatch matches)
    pure $ "int " <> newVar <> " = " <> scrutStr <> ";\n"
        <> unlines matchesStrs <> "\n"
  where
    toCmatch :: (Xp Bool, Xp a) -> R.Reader (Maybe String) String
    toCmatch (cond, body) = do
        condStr <- toC cond
        bodyStr <- toC body
        pure $ "if (" <> condStr <> ") { " <> bodyStr <> " }"
toC (Var s) = pure s
toC (Val v) = pure $ show v
toC (Add e1 e2) = do
    e1Str <- toC e1
    e2Str <- toC e2
    pure $ e1Str <> " + " <> e2Str
toC (Gt e1 e2) = do
    e1Str <- toC e1
    e2Str <- toC e2
    pure $ e1Str <> " > " <> e2Str
toC SymVar = R.ask >>= \case
    Nothing -> error "unknown SymVar reference"
    Just s -> pure s
