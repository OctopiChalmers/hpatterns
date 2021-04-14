{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module E.Compile where

import Data.Functor ((<&>))
import Lens.Micro
import Lens.Micro.Mtl

import E.Core
import E.CTypes

import qualified Control.Monad.State.Strict as St
import qualified Data.Map.Strict as M
import qualified Lens.Micro.TH


--
-- * Compilation utils
--

type Compile = St.State CompileState

newtype Name = Name
    { unName :: String
    } deriving newtype (Show)
      deriving stock (Eq, Ord)

-- | State during compilation.
data CompileState = CompileState
    { _csCounter :: Int
    -- ^ Seed for generating unique variables.
    , _csDefs    :: [String]
    -- ^ Function definitions added as necssary.
    , _csGlobals :: [String]
    -- ^ Global variable declarations.
    , _csStructs :: M.Map Name String
    -- ^ Definitions for structs. Maps name of struct (its type) to definition.
    }
$(Lens.Micro.TH.makeLenses ''CompileState)

-- ** Other utilities

-- | Return a unique identifier and increment the counter in state.
freshCid :: Compile Name
freshCid = do
    newId <- ('v' :) . show <$> use csCounter
    modifying csCounter (+ 1)
    pure $ Name newId

-- * Compilation

writeProg :: CType a => FilePath -> Estate (E a) -> IO ()
writeProg fp = writeFile fp . compile . runEstate

printProg :: CType a => Estate (E a) -> IO ()
printProg = putStrLn . compile . runEstate

compile :: forall a. CType a => E a -> String
compile expr =
    let (code, st) = runCompile (ce expr)
    in mconcat
        [ "\n// Code generated from Xp program \n\n"

        -- #include lines
        , concatMap ((++ "\n") . includeWrap)
            ["stdbool.h", "stdlib.h", "stdio.h", "math.h"], "\n"

        -- Define structs
        , "// Structs representing product types\n"
        , unlines (M.elems $ st ^. csStructs), "\n"

        -- Declare global variables
        , "// Variables correpsonding to scrutinees in case expressions\n"
        , unlines (st ^. csGlobals), "\n"

        -- The definitions are added in the wrong order (for the C code),
        -- so we reverse the list of definitions before printing them.
        , unlines . reverse $ (st ^. csDefs)
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
            , _csGlobals = []
            , _csStructs = M.empty
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

ce :: forall a . CType a => E a -> Compile String
ce expr = case expr of
    EVal v -> pure $ cval v
    EVar s -> pure s

    ESym s -> pure s
    ECase scrut@(Scrut e sName) matches -> do
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
newCaseDef (Scrut scrut sName) matches = do
    fName <- freshCid
    ifs <- cMatches matches

    let def = concat
            [ ctype @b, " ", unName fName, "(", ctype @a, " ", sName, ") {\n"
            , "    ", ctype @b, " ", resVar, ";\n"
            , concatMap (\ x -> "    " ++ x ++ "\n") ifs
            , "    return ", resVar, ";\n"
            , "}\n"
            ]
    modifying csDefs (def :)
    pure fName

  where
    resVar :: String
    resVar = "res"

    cMatches :: [Match p b] -> Compile [String]
    cMatches xs = do
        ifs <- mapM cMatch xs
        pure $ ifs ++ [nonMatch]
      where
        nonMatch :: String
        nonMatch = concat
            ["{ fprintf(stderr, \"No match on: `", sName, "`\\n\"); exit(1); }"]

        cMatch :: Match p b -> Compile String
        cMatch (Match cond _sop body) = do
            cond' <- ce cond
            body' <- ce body
            pure $ concat
                ["if (", cond', ") { ", resVar, " = ", body', "; } else "]
