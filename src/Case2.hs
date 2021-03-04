module Case2 where

import qualified Control.Monad.Trans.Reader as R

import HExp


data Vec = Vec
    { vecX :: Int
    , vecY :: Int
    } deriving stock Show
instance ProdType Vec where
    consName _ = "Vec"
    args v = [ConsArg TInt "vecX" (vecX v), ConsArg TInt "vecY" (vecY v)]
    arity = length . args
    argNames = map str (args @Vec (error "dummy vector"))
      where
        str :: ConsArg -> String
        str (ConsArg _ s _) = s

case2 ::
    forall a b .
    ( ProdType a
    , Show a
    )
    => HExp a                  -- ^ Scrutinee. Expected to be an HVar.
    -> (ConsArgs a -> HExp b)  -- ^ Function making use of type information and
                               -- argument names (not the values, though this
                               -- isn't enforced).
    -> HExp b
case2 scrut f = HCase2 scrut (f as)
  where
    as = args (Vec undefined undefined)

--
-- * Examples
--

{- | Example program. Represents something similar to:

> case e of
>   Vec x y -> x + y
-}
case2ex :: HExp Int
case2ex = case2 @Vec (HVar "e") addVecs
  where
    addVecs :: ConsArgs Vec -> HExp Int
    addVecs [ConsArg TInt x@"vecX" _, ConsArg TInt y@"vecY" _] =
        HDot @Vec HPVar x + HDot @Vec HPVar y

--
-- * Other
--

bind2 :: HExp a -> HExp a
bind2 (HCase2 scrut body) = HCase2 (HNamedExp "x" scrut) (bind2 body)
bind2 (HVar s) = HVar s
bind2 (HVal v) = HVal v
bind2 (HAdd e1 e2) = HAdd (bind2 e1) (bind2 e2)
bind2 (HDot (HPVar :: HExp a) s) = HDot (HVar @a "x") s

--
-- * Code generation
--

toC :: Show a => HExp a -> String
toC (HCase2 (HNamedExp name (scrut :: ProdType x => HExp x)) body) = "\n" <>
    structDef @x undefined <> "\n"
    <> funWrap (
        "struct " <> consName @Vec undefined <> " " <> name <> " = " <> scrut'
        <> ";\n"
        <> "return (" <> body' <> ")"
        )
  where
    funWrap :: String -> String
    funWrap s =
        "int somefunc(" <> consName @Vec undefined <> " " <> scrut' <> ")\n{\n"
        <> s
        <> "\n}\n"

    (scrut', body') :: (String, String) = (toC scrut, toC body)
toC (HVar s) = s
toC (HVal v) = show v
toC (HAdd e1 e2) = toC e1 <> " + " <> toC e2
toC (HDot (HVar s) fieldName) = s <> "." <> fieldName
