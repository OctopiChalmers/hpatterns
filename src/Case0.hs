{- | Pointless, "Case1" has the same contents but with a more generic
(but equally unsafe) case-function.
-}

module Case0 where

import qualified Control.Monad.Trans.State.Strict as St
import qualified Language.C99.Simple.AST as C
import qualified Language.C99.Simple
import qualified Language.C99 as C99
import qualified Language.C99.Pretty as C99.Pretty

import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint)

import HExp


{- | Works with partitions with exactly two constructors, which will
have exactly one argument. For example, the following can be (sort of?)
represented:

> case e of
>   Pos x -> x + 1
>   Neg x -> x

The conditions for taking the Pos/Neg branch is given by the toHExp class
method though, so really, it's more like this:

> case e of
>   x | x > 0 -> x + 1
>   x | 0 > x -> x
-}
case0 ::
    forall p a b .
    ( Show a
    , Partable p a  -- Partitioning exists for type a
    )
    => HExp a              -- ^ Scrutinee.
    -> (HExp a -> HExp b)  -- ^ As many functions as there are cases.
    -> (HExp a -> HExp b)  -- ^ As many functions as there are cases.
    -> HExp b
case0 scrut f1 f2 = HCase0 scrut (zip conditions [f1 HPVar, f2 HPVar])
  where
    partitions :: [p a]
    partitions = [minBound ..]

    conditions :: [HExp Bool]
    conditions = map toHExp partitions

{- | Example program. Represents the following (sort of?):
> case e of
>   Pos x -> x + 1
>   Neg x -> x
-}
case0ex0 :: HExp Int
case0ex0 = case0 @PatSign (HVar "e") pos neg
  where
    pos :: HExp Int -> HExp Int
    pos = (+ 1)

    neg :: HExp Int -> HExp Int
    neg = id

{- | Example program. Represents the following (sort of?):
> case e of
>   Pos _ -> True
>   Neg _ -> False
-}
case0ex1 :: HExp Bool
case0ex1 = case0 @PatSign (HVar "e") pos neg
  where
    pos :: HExp Int -> HExp Bool
    pos _ = HVal True

    neg :: HExp Int -> HExp Bool
    neg _ = HVal False

-- | Replace HPVars with HVars
bind0 :: HExp a -> HExp a
bind0 (HCase0 scrut cases) = HCase0 scrut (map bindCase cases)
  where
    bindCase :: (HExp Bool, HExp a) -> (HExp Bool, HExp a)
    bindCase (cond, body) = (bind0 cond, bind0 body)
bind0 HPVar = HVar "x"  -- hardcoded
bind0 (HVal v) = HVal v
bind0 (HVar s) = HVar s
bind0 (HAdd e1 e2) = HAdd (bind0 e1) (bind0 e2)
bind0 (HMul e1 e2) = HMul (bind0 e1) (bind0 e2)
bind0 (HGt e1 e2) = HGt (bind0 e1) (bind0 e2)

--
-- * Code generation
--

buildAST0 :: Show a => HExp a -> C.TransUnit
buildAST0 e = wrapper
  where
    wrapper :: C.TransUnit
    wrapper = C.TransUnit [] [fd1]

    fd1 :: C.FunDef
    fd1 = C.FunDef (C.TypeSpec C.Void) "fd1" [] [] stmts

    stmts :: [C.Stmt]
    stmts = toC0 e

    toC0 :: Show a => HExp a -> [C.Stmt]
    toC0 (HCase0 (HVar s) cases) = init : [cSwitch cases]
      where
        init :: C.Stmt
        init = C.Expr (C.InitVal (C.TypeName (C.TypeSpec C.Int)) [C.InitExpr (C.Ident s)])

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
            _ -> error $ "cExp: unexpected expression `" <> show e <> "`"

expToC0 :: Language.C99.Simple.TransUnit -> IO ()
expToC0 = print . pPrint . Language.C99.Simple.translate

instance Pretty C99.TransUnit where
    pPrint = C99.Pretty.pretty
