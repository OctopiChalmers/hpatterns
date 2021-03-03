module Case2 where

import qualified Control.Monad.Trans.Reader as R

import HExp


data Vec = Vec
    { vecX :: Int
    , vecy :: Int
    } deriving stock Show
instance ProdType Vec where
    consName _ = "Vec"
    args (Vec x y) = [ConsArg TInt "vecX" x, ConsArg TInt "vecY" y]
    arity = length . args

case2 ::
    forall a b .
    ( ProdType a
    , Show a
    )
    => HExp a  -- ^ Scrutinee
    -> (HExp a -> HExp b)
    -> HExp b
case2 scrut f = HCase2 scrut (f HPVar)

case2ex :: HExp Int
case2ex = case2 @Vec (HVar "e") addVecs
  where
    addVecs :: HExp Vec -> HExp Int
    addVecs v = HDot v "vecX" + HDot v "vecY"

bind2 :: forall a . ProdType a => HExp a -> R.Reader (Maybe a) (HExp a)
bind2 (HCase2 scrut body) = do
    body' <- bind2 body
    pure $ HCase2 scrut body'
bind2 (HVar s) = pure $ HVar s
bind2 (HVal v) = pure $ HVal v
bind2 (HAdd e1 e2) = HAdd <$> bind2 e1 <*> bind2 e2
