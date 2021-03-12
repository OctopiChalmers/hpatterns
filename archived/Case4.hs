{- | Same as Case1, but with a separate class for sum type
constructors/branches, not using the Partable class.
-}

module Case4 where

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
    deriving (Show, Bounded, Enum)

instance SumCons T where
    cond :: T -> HExp Bool
    cond T1 = HPVar `HEq` HVal 0 `HOr` (HVal 0 `HGt` HPVar)
    cond T2 = (HPVar `HGt` HVal 0) `HAnd` (HVal 10 `HGt` HPVar)
    cond T3 = (HVal 0 `HGt` HPVar) `HOr` (HPVar `HGt` HVal 10)

case4 :: forall c a b .
    ( SumCons c
    , Show a
    )
    => HExp a
    -> [HExp a -> HExp b]
    -> HExp b
case4 scrut fs = HCase0 scrut (zip conditions bodies)
  where
    bodies :: [HExp b]
    bodies = map ($ HPVar) fs

    constructors :: [c]
    constructors = [minBound ..]

    conditions :: [HExp Bool]
    conditions = map cond constructors

case4ex :: HExp Bool
case4ex = case4 @T (HVar "e") [zero, small, large]
  where
    zero :: HExp Int -> HExp Bool
    zero = const (HVal True)

    small :: HExp Int -> HExp Bool
    small = const (HVal False)

    large :: HExp Int -> HExp Bool
    large = const (HVal True)

-- | Replace HPVars with HVars. Does not support nested case-expressions.
bind4 :: HExp a -> HExp a
bind4 (HCase0 scrut cases) = HCase0 (HNamedExp "x" scrut) (map bindCase cases)
  where
    bindCase :: (HExp Bool, HExp a) -> (HExp Bool, HExp a)
    bindCase (cond, body) = (bind4 cond, bind4 body)
bind4 HPVar = HVar "x"  -- hardcoded
bind4 (HVal v) = HVal v
bind4 (HVar s) = HVar s
bind4 (HAdd e1 e2) = HAdd (bind4 e1) (bind4 e2)
bind4 (HMul e1 e2) = HMul (bind4 e1) (bind4 e2)
bind4 (HGt e1 e2) = HGt (bind4 e1) (bind4 e2)
bind4 (HEq e1 e2) = HEq (bind4 e1) (bind4 e2)
bind4 (HAnd e1 e2) = HAnd (bind4 e1) (bind4 e2)

-- | Build a C AST representation of an HExp.
buildAst4 :: Show a => HExp a -> C.TransUnit
buildAst4 e = wrapper
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
        HEq e1 e2 -> C.BinaryOp C.Eq (cExp e1) (cExp e2)
        _ -> error $ "cExp: unexpected expression `" <> show e <> "`"

{- | Use pretty library and pretty instance from language-c99 to print
somewhat legit-looking C code from a language-c99-simple AST.
-}
printAst4 :: Language.C99.Simple.TransUnit -> IO ()
printAst4 = print . pPrint . Language.C99.Simple.translate

instance Pretty C99.TransUnit where
    pPrint = C99.Pretty.pretty
