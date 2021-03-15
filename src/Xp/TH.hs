{-# LANGUAGE TemplateHaskell #-}

module Xp.TH where

import qualified Data.Text.Lazy as T
import qualified Data.List as List

import qualified Xp.Core

import Language.Haskell.TH
import Language.Haskell.TH.Syntax


makeConstructors :: Name -> Q Exp
makeConstructors pa = do
    TyConI typeDec <- reify pa
    let DataD _ _ _ _ ddCons _ = typeDec
    ListE <$> createCons ddCons
  where
    createCons :: [Con] -> Q [Exp]
    createCons cons = mapM con2exp cons

    con2exp :: Con -> ExpQ
    con2exp (NormalC name btys) = do
        exps <- mapM bt2arg btys
        return $ apply (ConE name) exps
      where
        apply :: Exp -> [Exp] -> Exp
        apply e = List.foldl' AppE e

    bt2arg :: BangType -> ExpQ
    bt2arg (bang, ty) =
        case ty of
            AppT (ConT name) t2
                | name == ''Xp.Core.Xp -> [e| (Xp.Core.SVar) |]
            _ -> error $ "Non-Xp argument to data constructor: " <> show ty
