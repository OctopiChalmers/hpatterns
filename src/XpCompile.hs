{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module XpCompile where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as St
import qualified Data.Map.Strict as M
import qualified Lens.Micro as Lens
import qualified Lens.Micro.Mtl as Lens.Mtl
import qualified Lens.Micro.TH as Lens.TH

import Xp


--
-- * Compilation utils
--

type Compile = R.ReaderT Name (St.State Cm)

newtype Name = Name
    { unName :: String
    } deriving newtype (Show)
      deriving stock (Eq, Ord)

-- | State during compilation.
data Cm = Cm
    { _cmCounter :: Int
    -- ^ Seed for generating unique variables.
    , _cmDefs    :: [String]
    -- ^ Function definitions added as necssary.
    , _cmStructs :: M.Map Name String
    -- ^ Struct definitions (name of struct -> definition of struct).
    }
$(Lens.TH.makeLenses ''Cm)

-- ** Other utilities

-- | Return a unique identifier and increment the counter in state.
freshId :: Compile Name
freshId = do
    newId <- ('v' :) . show <$> Lens.Mtl.use cmCounter
    Lens.Mtl.modifying cmCounter (+ 1)
    pure $ Name newId

--
-- * Main compilation stuff
--

{- | Main entry point for compilation. Compile an 'Xp.Xp' program (expression)
into its C representation.
-}
compile :: Show a => Xp a -> String
compile program =
    let (code, st) :: (String, Cm) =
            St.runState (R.runReaderT (cXp program) initName) initCm
        main = mainWrap code
    in mconcat
        [ "\n// Code generated from Xp program \n\n"
        , concatMap (++ "\n") (M.elems (st Lens.^. cmStructs))
        , concatMap (++ "\n") (st Lens.^. cmDefs)
        , main
        ]
  where
    initName :: Name
    initName = Name "scrut"

    initCm :: Cm
    initCm = Cm
        { _cmCounter = 0
        , _cmDefs = []
        , _cmStructs = M.empty
        }

    mainWrap :: String -> String
    mainWrap body = mconcat
        [ "int main() {\n"
        , "    int output = ", body, ";\n"
        , "    printf(\"Program output is: %d\", output)\n"
        , "    return 0;\n"
        , "}\n"
        ]

{- | Compile an expression into its C representation. Needed function
definitions are generated as needed and added to the compilation state.
-}
cXp :: Show a => Xp a -> Compile String
cXp = \case
    Val v -> pure $ show v
    Var s -> pure $ show s
    Add e1 e2 -> binOp "+" e1 e2
    Mul e1 e2 -> binOp "*" e1 e2
    Sub e1 e2 -> binOp "-" e1 e2
    Gt  e1 e2 -> binOp ">" e1 e2
    Lt  e1 e2 -> binOp "<" e1 e2
    Eq  e1 e2 -> binOp "==" e1 e2
    And e1 e2 -> binOp "&&" e1 e2
    Or  e1 e2 -> binOp "||" e1 e2
    Not e -> ("(-" ++) . (++ ")") <$> cXp e

    Case scrut matches -> do
        funName <- freshId
        newCaseFun funName matches

        scrutStr <- cXp scrut
        pure $ mconcat [unName funName, "(", scrutStr, ")"]

    CaseP (scrut :: Xp pt) body -> do
        newStruct @pt
        funName <- freshId
        newCasePFun funName body

        scrutStr <- cXp scrut
        pure $ mconcat [unName funName, "(", scrutStr, ")"]

    SVar -> unName <$> R.ask
    SField s -> do
        scrutStr <- unName <$> R.ask
        pure $ scrutStr <> "." <> s

    exp -> error $ "cXp: unexpected case `" <> show exp <> "`"
  where
    binOp :: (Show a, Show b) => String -> Xp a -> Xp b -> Compile String
    binOp op e1 e2 = do
        s1 <- cXp e1
        s2 <- cXp e2
        pure $ mconcat ["(", s1, " ", op, " ", s2, ")"]

{- | Convert the body of a sum pattern match ('Xp.Case') to a new function
definition, and add it to the compilation state.
-}
newCaseFun :: forall b .
    ( Show b
    )
    => Name               -- ^ Name of function.
    -> [(Xp Bool, Xp b)]  -- ^ Branches/matches of case-expression.
    -> Compile ()
newCaseFun (Name funName) matches = do
    resVar <- freshId
    matchesStr <- mapM (cMatch resVar) matches
    Name scrutName <- R.ask

    let def = mconcat
            [ "int ", funName, "(int ", scrutName, ") {\n"
            , "    int ", unName resVar, ";\n"
            , concatMap ("    " ++) matchesStr
            , "    return ", unName resVar, ";\n"
            , "}\n"
            ]
    Lens.Mtl.modifying cmDefs (def :)
  where
    cMatch :: Name -> (Xp Bool, Xp b) -> Compile String
    cMatch (Name resVar) (cond, body) = do
        condStr <- cXp cond
        bodyStr <- cXp body
        pure $ mconcat ["if (", condStr, ") { ", resVar, " = ", bodyStr, " }\n"]

newCasePFun :: forall b .
    ( Show b
    )
    => Name
    -> Xp b
    -> Compile ()
newCasePFun (Name funName) body = do
    resVar <- freshId
    bodyStr <- cXp body
    Name scrutName <- R.ask

    let def = mconcat
            -- TODO: Haven't properly included translation of the field type yet,
            -- hence the placeholder.

            -- Also, literal syntax for structs needs to be fixed (compound
            -- literals); right now, a Val (V x y) will just be printed as the
            -- Haskell representation.
            [ "int ", funName, "(PLACEHOLDER_TYPE ", scrutName, ") {\n"
            , "    PLACEHOLDER_TYPE ", unName resVar, " = ", bodyStr, ";\n"
            , "    return ", unName resVar, ";\n"
            , "}\n"
            ]
    Lens.Mtl.modifying cmDefs (def :)

{- | Add a new structure definition to the compilation state, if it doesn't
already exist.
-}
newStruct :: forall a .
    ( Show a
    , ProdType a
    )
    => Compile ()
newStruct = do
    let structName = consName @a
    let fieldsStr = map cField (absArgs @a)
    let def = mconcat
            [ "struct ", structName, " {\n"
            , concatMap ("    " ++) fieldsStr
            , "};\n"
            ]
    Lens.Mtl.modifying cmStructs (insertIfNew (Name structName) def)
  where
    cField :: AbsField -> String
    cField (AbsField t s) = mconcat [showType t, " ", s, ";\n"]

    insertIfNew :: Ord k => k -> v -> M.Map k v -> M.Map k v
    insertIfNew = M.insertWith (\ _ y -> y)
