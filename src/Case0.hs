module Case0 where

import qualified Language.C99.Simple.AST as C

import Language.C99.Pretty (Pretty (..))

import HExp


{- | Works with partitions with exactly two constructors, which will
have exactly one argument. For example, the following can be (sort of?)
represented:

> case e of
>   Pos x -> x + 1
>   Neg x -> x
-}
case0 ::
    forall a b p .
    ( Show a
    , Partable p a  -- Partitioning exists for type a
    )
    => HExp a                -- ^ Scrutinee.
    -> (HExp a -> HExp b)    -- ^ As many functions as there are cases.
    -> (HExp a -> HExp b)    -- ^ As many functions as there are cases.
    -> HExp b
case0 scrut f1 f2 = HCase0 scrut (zip partitions [f1 HPVar, f2 HPVar])
  where
    partitions :: [p a]
    partitions = [minBound ..]

{- | Example program. Represents the following (sort of?):

> case e of
>   Pos x -> x + 1
>   Neg x -> x
-}
case0ex :: HExp Int
case0ex = case0 @Int @Int @PatSign (HVar "e") pos neg
  where
    pos :: HExp Int -> HExp Int
    pos = (+ 1)

    neg :: HExp Int -> HExp Int
    neg = id

-- | Replace HPVars with HVars. Does not support nested case-expressions.
-- bind0 :: (Show a, Partable p a) => HExp a -> HExp a
-- bind0 :: forall a b p . (Show a, Partable p a) => HExp a -> HExp a
bind0 :: (Show a, Partable p a) => HExp a -> HExp a
bind0 (HCase0 scrut cases) = HCase0 (bind0 scrut) undefined -- undefined (map bindCase cases)
bind0 HPVar    = HVar "x"  -- Hardcoded
bind0 (HVar s) = HVar s
bind0 (HVal v) = HVal v
-- bind0 (HCase0 scrut cases) = HCase0 @a @p undefined undefined
--   where
--     bindCase :: (Partable p a) => (p a, HExp b) -> (p a, HExp b)
--     bindCase (pa, body) = (pa, bind0 body)
bind0 e = error $ "bind0: unexpected expression `" <> show e <> "`"

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

    cSwitch :: (Partable p a) => [(p a, HExp b)] -> C.Stmt
    cSwitch cases = C.Switch (C.Ident s) (map cCase cases)

    cCase :: (Partable p a) => (p a, HExp b) -> C.Case
    cCase (pa, e) = undefined



