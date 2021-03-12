{- | Improvement on Case4, making the case function slighly safer.
-}

module Case5 where

import qualified Data.Map.Strict as M
import qualified Data.List as List
import qualified Language.C99.Simple.AST as C
import qualified Language.C99.Simple
import qualified Language.C99 as C99
import qualified Language.C99.Pretty as C99.Pretty

import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint)

import HExp


class (Bounded c, Enum c) => SumCons c where
    -- | We need to know how to choose a specific constructor/branch.
    cond :: c -> HExp Bool

data T = T1 | T2 | T3
    deriving (Show, Bounded, Enum, Eq, Ord)

instance SumCons T where
    cond :: T -> HExp Bool
    cond T1 = HPVar `HEq` HVal 0 `HOr` (HVal 0 `HGt` HPVar)
    cond T2 = (HPVar `HGt` HVal 0) `HAnd` (HVal 10 `HGt` HPVar)
    cond T3 = (HVal 0 `HGt` HPVar) `HOr` (HPVar `HGt` HVal 10)

case5 :: forall c a b .
    ( SumCons c
    , Ord c
    , Show c
    , Show a
    )
    => HExp a                   -- ^ Scrutinee.
    -> [(c, HExp a -> HExp b)]  -- ^ Function for each constructor.
    -> HExp b
case5 scrut fs = HCase0 scrut (M.elems matches)
  where
    matches :: M.Map c (HExp Bool, HExp b)
    matches = go fs M.empty conditions
      where
        go ::
               [(c, HExp a -> HExp b)]
            -> M.Map c (HExp Bool, HExp b)
            -> M.Map c (HExp Bool)
            -> M.Map c (HExp Bool, HExp b)
        go [] res m =
            if M.size res == M.size m
            then res
            else error
                $ "case5: mismatch between number of functions and cases, "
                <> "constructors for cases are: "
                <> concatMap (("\n" ++) . show) ([minBound ..] :: [c])
        go ((c, f) : xs) res m =
            let body = f HPVar :: HExp b
                cond = m M.! c :: HExp Bool
            in go xs (M.insert c (cond, body) res) m

    constructors :: [c]
    constructors = [minBound ..]

    conditions :: M.Map c (HExp Bool)
    conditions = M.fromList $ map (\ c -> (c, cond c)) constructors

case5ex :: HExp Bool
case5ex = case5 @T (HVar "e") [(T1, zero), (T2, small), (T3, large)]
  where
    zero :: HExp Int -> HExp Bool
    zero = const (HVal True)

    small :: HExp Int -> HExp Bool
    small = const (HVal False)

    large :: HExp Int -> HExp Bool
    large = const (HVal True)

-- | Replace HPVars with HVars. Does not support nested case-expressions.
bind5 :: HExp a -> HExp a
bind5 (HCase0 scrut cases) = HCase0 (HNamedExp "x" scrut) (map bindCase cases)
  where
    bindCase :: (HExp Bool, HExp a) -> (HExp Bool, HExp a)
    bindCase (cond, body) = (bind5 cond, bind5 body)
bind5 HPVar = HVar "x"  -- hardcoded
bind5 (HVal v) = HVal v
bind5 (HVar s) = HVar s
bind5 (HAdd e1 e2) = HAdd (bind5 e1) (bind5 e2)
bind5 (HMul e1 e2) = HMul (bind5 e1) (bind5 e2)
bind5 (HGt e1 e2) = HGt (bind5 e1) (bind5 e2)
bind5 (HEq e1 e2) = HEq (bind5 e1) (bind5 e2)
bind5 (HAnd e1 e2) = HAnd (bind5 e1) (bind5 e2)
bind5 (HOr e1 e2) = HOr (bind5 e1) (bind5 e2)

-- | Build a C AST representation of an HExp.
buildAst5 :: Show a => HExp a -> C.TransUnit
buildAst5 e = wrapper
  where
    wrapper :: C.TransUnit
    wrapper = C.TransUnit [] [fd1]

    fd1 :: C.FunDef
    fd1 = C.FunDef (C.TypeSpec C.Void) "fd1" [] [] stmts

    stmts :: [C.Stmt]
    stmts = cStmts e

    cStmts :: Show a => HExp a -> [C.Stmt]
    cStmts (HCase0 (HNamedExp s scrut) cases) = init : [cSwitch cases]
      where
        init :: C.Stmt
        init = C.Expr
            (C.InitVal (C.TypeName (C.TypeSpec C.Int))
            [C.InitExpr $ C.AssignOp C.Assign (C.Ident s) (cExp scrut)])

        cSwitch :: (Show a) => [(HExp Bool, HExp a)] -> C.Stmt
        cSwitch cases = C.Switch (C.Ident s) (map cCase cases)

        cCase :: (Show a) => (HExp Bool, HExp a) -> C.Case
        cCase (pa, e) = C.Case (cExp pa) (C.Expr (cExp e))

    cExp :: Show a => HExp a -> C.Expr
    cExp e = case e of
        HVal v -> C.Ident ("VAL_" ++ show v)
        HVar s -> C.Ident s
        HAdd e1 e2 -> C.BinaryOp C.Add (cExp e1) (cExp e2)
        HGt e1 e2 -> C.BinaryOp C.GT (cExp e1) (cExp e2)
        HAnd e1 e2 -> C.BinaryOp C.LAnd (cExp e1) (cExp e2)
        HOr e1 e2 -> C.BinaryOp C.LOr (cExp e1) (cExp e2)
        HEq e1 e2 -> C.BinaryOp C.Eq (cExp e1) (cExp e2)
        _ -> error $ "cExp: unexpected expression `" <> show e <> "`"

{- | Use pretty library and pretty instance from language-c99 to print
somewhat legit-looking C code from a language-c99-simple AST.
-}
printAst5 :: Language.C99.Simple.TransUnit -> IO ()
printAst5 = print . pPrint . Language.C99.Simple.translate

instance Pretty C99.TransUnit where
    pPrint = C99.Pretty.pretty
