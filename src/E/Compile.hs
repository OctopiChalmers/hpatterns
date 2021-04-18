{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module E.Compile where

import Control.Arrow ((<<<))
import Control.Monad (when)
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Proxy (Proxy (..))
import Lens.Micro ((^.), _head)
import Lens.Micro.Mtl (assign, modifying, use)

import E.Core
import E.CTypes (CType (..))

import qualified Control.Monad.State.Strict as St
import qualified Data.Map.Strict as M
import qualified Lens.Micro.TH


--
-- * Compilation utils
--

newtype Name = Name
    { unName :: String
    } deriving newtype (Show)
      deriving stock (Eq, Ord)

{- | 'EC' is a wrapper for 'E' for use where homogeneity is needed and only
the 'CType' methods are required.
-}
data EC = forall a . CType a => EC (E a)

{- | Global variable declaration representations are parameterized by their
C-representable type.
-}
data Global a = forall t . CType t => Global (Proxy t) a

type Compile = St.State CompileState

-- | State during compilation.
data CompileState = CompileState
    { _csCounter :: Int
    -- ^ Counter for generating unique names for stuff.
    , _csDefs :: [String]
    -- ^ Function definitions as whole strings. Accumulated during compilation.
    , _csGlobalScrutIds :: [Global ScrutId]
    -- ^ Global variables for scrutinees.
    , _csGlobalArgIds :: [Global ArgId]
    -- ^ Global variables for constructor fields.
    , _csCtxts :: [M.Map ArgId EC]
    -- ^ Contexts for case-of's/pattern matches, modelled as a stack. Each field
    -- of a constructor is represented by a unique ID, along with the ID of the
    -- scrutinee it originated from. A new map is used for each new pattern
    -- match construct. The mapping to a (wrapped) 'E' allows us to bind
    -- the expressions of constructor fields to variables, so they can be
    -- reused internally.
    }
$(Lens.Micro.TH.makeLenses ''CompileState)

-- ** Other utilities

-- | Return a unique identifier and increment the counter in state.
freshCid :: Compile Name
freshCid = do
    newId <- ('v' :) . show <$> use csCounter
    modifying csCounter (+ 1)
    pure $ Name newId

showArgId :: ArgId -> String
showArgId (ArgId scrutId (FieldId fid)) =
    concat [showScrutId scrutId, "_field", show fid]

showScrutId :: ScrutId -> String
showScrutId (ScrutId sid) = "_scrut" ++ show sid

showGlobalScrutId :: Global ScrutId -> String
showGlobalScrutId (Global (_ :: Proxy t) x) =
    concat [ctype @t, " ", showScrutId x, ";"]

showGlobalArgId :: Global ArgId -> String
showGlobalArgId (Global (_ :: Proxy t) x) =
    concat [ctype @t, " ", showArgId x, ";"]

--
-- * Compilation
--

-- | Compile and write the generated C code to file.
writeProg :: CType a => FilePath -> Estate (E a) -> IO ()
writeProg fp = writeFile fp . compile . runEstate

-- | Compile and output the generated C code to stdout.
printProg :: CType a => Estate (E a) -> IO ()
printProg = putStrLn . compile . runEstate

{- | Main entry point for compilation. Generate code for an @'E' a@ expression
where @a@ is a type that can be represented in C (indicated by the 'CType')
constraint.
-}
compile :: forall a. CType a => E a -> String
compile expr =
    let (code, st) = runCompile (ce expr)
    in mconcat
        [ "\n// Code generated from E program \n\n"

        -- #include lines
        , concatMap ((++ "\n") . includeWrap)
            ["stdbool.h", "stdlib.h", "stdio.h", "math.h"], "\n"

        -- Insert global variable declarations.
        , "// Global variables for scrutinees.\n"
        , unlines . map showGlobalScrutId . sortOn (\ (Global _ x) -> x)
            $ st ^. csGlobalScrutIds
        , "\n"
        , "// Global variables for constructor fields.\n"
        , unlines . map showGlobalArgId . sortOn (\ (Global _ x) -> x)
            $ st ^. csGlobalArgIds
        , "\n"

        -- Function definitions are added in the wrong order (for the C code),
        -- so we reverse the list of definitions before printing them.
        , unlines . reverse $ (st ^. csDefs)

        -- Create the main() function entry point.
        -- NOTE: Doesn't handle any input currently.
        , mainWrap code
        ]
  where
    runCompile :: Compile a1 -> (a1, CompileState)
    runCompile = flip St.runState initCompileState
      where
        initCompileState :: CompileState
        initCompileState = CompileState
            { _csCounter = 0
            , _csDefs = []
            , _csGlobalScrutIds = []
            , _csGlobalArgIds = []
            , _csCtxts = []
            }

    includeWrap :: String -> String
    includeWrap s = "#include <" ++ s ++ ">"

    mainWrap :: String -> String
    mainWrap body = mconcat
        [ "int main() {\n"
        , "    ", ctype @a, " output = ", body, ";\n"
        , "    printf(\"Output: %", cformat @a : "\\n\", output);\n"
        , "    return 0;\n"
        , "}\n"
        ]

-- | Add a new empty map to the constructor field context stack.
pushCtxt :: Compile ()
pushCtxt = modifying csCtxts (M.empty :)

-- | Return the top constructor field context and decrease the stack.
popCtxt :: Compile (M.Map ArgId EC)
popCtxt = use csCtxts >>= \case
    []     -> error "popEnv: Empty stack"
    x : xs -> assign csCtxts xs >> pure x

-- | Serialize an 'E' expression and affect the compilation state as necessary.
ce :: forall rt . CType rt => E rt -> Compile String  -- "rt" for "return type"
ce expr = case expr of
    EVal v -> pure $ cval v
    EVar s -> pure s

    EField argId e -> do
        -- Add the ArgId of the expression we encountered to the current
        -- context, so that the outer 'ECase' construct knows to bind it.
        modifying (csCtxts <<< _head) (M.insert argId (EC e))

        -- And if it's the first time we made use of this field, we need to
        -- add add a global variable declaration for it.
        isNew <- not <$> globalArgIdExists argId
        when isNew (newGlobalArgId @rt $ argId)

        pure $ showArgId argId

    ESym s -> pure $ showScrutId s

    ECase scrut@(Scrut e _s) matches -> do
        fName <- newCaseDef scrut matches
        scrutStr <- ce e
        pure $ concat [unName fName, "(", scrutStr, ")"]

    EAdd e1 e2 -> binOp e1 e2 "+"
    EMul e1 e2 -> binOp e1 e2 "*"
    ESub e1 e2 -> binOp e1 e2 "-"
    EDiv e1 e2 -> binOp e1 e2 "/"
    EGt  e1 e2 -> binOp e1 e2 ">"
    ELt  e1 e2 -> binOp e1 e2 "<"
    EGte e1 e2 -> binOp e1 e2 ">="
    ELte e1 e2 -> binOp e1 e2 "<="
    EEq  e1 e2 -> binOp e1 e2 "=="
    EAnd b1 b2 -> binOp b1 b2 "&&"
    EOr  b1 b2 -> binOp b1 b2 "||"

    ENot b -> ce b <&> \ b' -> concat ["!(", b', ")"]

    ECFloorInt d    -> ce d <&> \ d' -> concat ["((int) floor(", d', "))"]
    ECFloorDouble d -> ce d <&> \ d' -> concat ["(floor(", d', "))"]

  where
    binOp :: (CType a1, CType a2)
        => E a1
        -> E a2
        -> String  -- ^ Operator as a string
        -> Compile String
    binOp e1 e2 op = do
        e1' <- ce e1
        e2' <- ce e2
        pure $ concat ["(", e1', " ", op, " ", e2', ")"]

{- | Create a function definition representing a case-of use and add
it to the compilation state. Return the function name.
-}
newCaseDef :: forall p a b . (CType a, CType b)
    => Scrut a
    -> [Match p b]
    -> Compile Name
newCaseDef (Scrut _scrutExp scrutId) matches = do
    pushCtxt

    newGlobalScrutId @a scrutId
    fName <- freshCid
    ifs <- cMatches matches

    -- At this point, the top of the csCtxts stack should contain the variables
    -- and expressions we need to bind.
    -- TODO: Creates redundant assignments when using nested cases where
    -- the inner case refers to a bound variable from the outer case.
    bindings <- mapM cScrutBinding . M.assocs =<< popCtxt

    let def = concat
            [ ctype @b, " ", unName fName, "(", ctype @a, " ", argName, ") {\n"
            , "    ", showScrutId scrutId, " = ", argName, ";\n"
            , "    ", ctype @b, " ", resName, ";\n"
            , concatMap (\ x -> "    " ++ x ++ "\n") bindings
            , concatMap (\ x -> "    " ++ x ++ "\n") ifs
            , "    return ", resName, ";\n"
            , "}\n"
            ]
    modifying csDefs (def :)

    pure fName

  where
    resName :: String
    resName = "res"

    argName :: String
    argName = "arg"

    cScrutBinding :: (ArgId, EC) -> Compile String
    cScrutBinding (argId, EC (e :: E t)) = do
        e' <- ce e
        pure $ concat [showArgId argId, " = ", e', ";"]

    cMatches :: [Match p b] -> Compile [String]
    cMatches xs = do
        ifs <- mapM cMatch xs
        pure $ ifs ++ [nonMatch]
      where
        nonMatch :: String
        nonMatch = concat
            [ "{ fprintf(stderr, \"No match on: `", showScrutId scrutId
            , "`\\n\"); exit(1); }"]

        cMatch :: Match p b -> Compile String
        cMatch (Match cond body) = do
            cond' <- ce cond
            body' <- ce body
            pure $ concat
                ["if (", cond', ") { ", resName, " = ", body', "; } else "]

{- | Add a global variable to the compilation state, for holding the value
of a constructor field.
-}
newGlobalArgId :: forall a . CType a => ArgId -> Compile ()
newGlobalArgId aid = modifying csGlobalArgIds (Global (Proxy @a) aid :)

{- | Add a global variable to the compilation state, for holding the value
of a scrutinee.
-}
newGlobalScrutId :: forall a . CType a => ScrutId -> Compile ()
newGlobalScrutId sid = modifying csGlobalScrutIds (Global (Proxy @a) sid :)

{- | Check if an ArgId is already a declared global varaible, regardless
of variable type.
-}
globalArgIdExists :: ArgId -> Compile Bool
globalArgIdExists aid =
    (aid `elem`) . map (\ (Global _ a) -> a) <$> use csGlobalArgIds
