{- | Some sort of case-expression representation.
To print the C-representation of an example:

@ 'printAst1' $ 'buildAst1' $ 'bind1' $ case1ex0 @
-}

module Case1 where

import qualified Control.Monad.Trans.State.Strict as St
import qualified Language.C99.Simple.AST as C
import qualified Language.C99.Simple
import qualified Language.C99 as C99
import qualified Language.C99.Pretty as C99.Pretty

import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint)

import HExp


--
-- * Combinators
--


{- | Builds an HExp representation of a basic case-expression on a sum type
Freeloads on the Partable type class, but probably any Enum type should
work.

For example, the following can be (sort of?) represented:

> case e of
>   Pos x -> x + 1
>   Neg x -> x

The conditions for taking the Pos/Neg branch is given by the toHExp class
method though, so really, it's more like this:

> case e of
>   x | x > 0 -> x + 1
>   x | 0 > x -> x

__Type safety:__

* Assumes that the number of functions provided is the same number as
the number of constructors for the 'Partable' type. Probably does something
weird if this is not the case.

* There is no way to explicitly specify which function to apply for which
case. Instead, the order used for the 'Partable' constructors is the one
given by deriving 'Enum'. The first function given to 'case1' is applied
to the first constructor, and so on.

For example, if we have the following definition for 'PatSign':

> data Num a => PatSign a = Pos | Neg
>     deriving (Bounded, Enum, Show)

and the expression

> case1 e [f1, f2]

where @e@ is a 'PatSign' type, @f1@ would be applied to the Pos
case, and @f2@ to the Neg case.
-}
case1 ::
    forall p a b .
    ( Show a
    , Partable p a  -- Partitioning exists for type a
    )
    => HExp a              -- ^ Scrutinee.
    -> [HExp a -> HExp b]  -- ^ As many functions as there are cases.
    -> HExp b
case1 scrut fs = HCase0 scrut (zip conditions bodies)
  where
    partitions :: [p a]
    partitions = [minBound ..]

    bodies :: [HExp b]
    bodies = map ($ HPVar) fs

    conditions :: [HExp Bool]
    conditions = map toHExp partitions

--
-- * Examples
--

{- | Example program. Represents the following (sort of?):

> case e of
>   Pos x -> x + 1
>   Neg x -> x
-}
case1ex0 :: HExp Int
case1ex0 = case1 @PatSign (HVar "e") [pos, neg]
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
case1ex1 :: HExp Bool
case1ex1 = case1 @PatSign (HVar "e") [pos, neg]
  where
    pos :: HExp Int -> HExp Bool
    pos _ = HVal True

    neg :: HExp Int -> HExp Bool
    neg _ = HVal False

--
-- * Other
--

-- | Replace HPVars with HVars. Does not support nested case-expressions.
bind1 :: HExp a -> HExp a
bind1 (HCase0 scrut cases) = HCase0 (HNamedExp "x" scrut) (map bindCase cases)
  where
    bindCase :: (HExp Bool, HExp a) -> (HExp Bool, HExp a)
    bindCase (cond, body) = (bind1 cond, bind1 body)
bind1 HPVar = HVar "x"  -- hardcoded
bind1 (HVal v) = HVal v
bind1 (HVar s) = HVar s
bind1 (HAdd e1 e2) = HAdd (bind1 e1) (bind1 e2)
bind1 (HMul e1 e2) = HMul (bind1 e1) (bind1 e2)
bind1 (HGt e1 e2) = HGt (bind1 e1) (bind1 e2)

--
-- * Code generation
--

-- | Build a C AST representation of an HExp.
buildAst1 :: Show a => HExp a -> C.TransUnit
buildAst1 e = wrapper
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
        _ -> error $ "cExp: unexpected expression `" <> show e <> "`"

{- | Use pretty library and pretty instance from language-c99 to print
somewhat legit-looking C code from a language-c99-simple AST.
-}
printAst1 :: Language.C99.Simple.TransUnit -> IO ()
printAst1 = print . pPrint . Language.C99.Simple.translate

instance Pretty C99.TransUnit where
    pPrint = C99.Pretty.pretty
