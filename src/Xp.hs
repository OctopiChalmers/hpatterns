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
    Sub :: (Num a) =>         Xp a -> Xp a -> Xp a
    Gt  :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
    Lt  :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
    Eq  :: (Show a, Eq a) =>  Xp a -> Xp a -> Xp Bool
    And ::                    Xp Bool -> Xp Bool -> Xp Bool
deriving stock instance Show a => Show (Xp a)

instance Num a => Num (Xp a) where
    fromInteger n = Val (fromInteger n)
    e1 + e2 = Add e1 e2
    e1 - e2 = Sub e1 e2

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

-- | @<@ for Xp.
(<.) :: (Show a, Num a) => Xp a -> Xp a -> Xp Bool
(<.) = flip Gt

-- | @==@ for Xp.
(==.) :: (Show a, Eq a) => Xp a -> Xp a -> Xp Bool
(==.) = Eq

xvar :: String -> Xp a
xvar = Var

xval :: a -> Xp a
xval = Val

--
-- * Prettyprinting
--

pPrintXp :: Show a => Xp a -> IO ()
pPrintXp e = putStrLn $ R.runReader (prettyXp e) initEnv
  where
    initEnv :: Env
    initEnv = Env Nothing Nothing

data Env = Env
    { envScrut :: Maybe String
    , envRes :: Maybe String
    }

prettyXp :: forall a .
    ( Show a
    )
    => Xp a
    -> R.Reader Env String  -- Env stores the name of the scrutinee
prettyXp e = case e of
    Var s -> pure s
    Val v -> pure $ show v
    Add e1 e2 -> binOp "+" e1 e2
    Sub e1 e2 -> binOp "-" e1 e2
    Gt e1 e2  -> binOp ">" e1 e2
    Eq e1 e2  -> binOp "==" e1 e2
    SymVar -> R.asks envScrut >>= \case
        Nothing -> error "no scrutinee variable in environment"
        Just s  -> pure s
    Case scrut matches -> do
        let newVar = "scrut"  -- hardcoded
        let resVar = "result"
        let newEnv = Env
                { envScrut = Just newVar
                , envRes   = Just resVar
                }
        scrutStr <- prettyXp scrut
        matchesStrs <- R.local (const newEnv) (mapM prettyXpMatch matches)
        pure $ "{\n"
            <> "int " <> newVar <> " = " <> scrutStr <> ";\n"
            <> "int " <> resVar <> ";\n"
            <> unlines matchesStrs
            <> "}\n"
  where
    binOp :: (Show x, Show y) => String -> Xp x -> Xp y -> R.Reader Env String
    binOp opStr e1 e2 = do
        e1Str <- prettyXp e1
        e2Str <- prettyXp e2
        pure $ unwords [e1Str, opStr, e2Str]

    prettyXpMatch :: (Xp Bool, Xp a) -> R.Reader Env String
    prettyXpMatch (cond, body) = do
        condStr <- prettyXp cond
        bodyStr <- prettyXp body
        resVar <- R.asks envRes >>= \case
            Nothing -> error "no return variable in environment"
            Just s -> pure s
        pure $ "if (" <> condStr <> ") { " <> resVar <> " = " <> bodyStr <> " }"
