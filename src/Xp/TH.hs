{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Xp.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Text.Pretty.Simple
import qualified Data.Text.Lazy as T

import Xp.Core

import Debug.Trace


test :: Name -> Q Exp
test pa = do
    TyConI typeDec <- reify pa
    traceM
        $ T.unpack
        $ pShowOpt (defaultOutputOptionsDarkBg{ outputOptionsCompact = False })
        typeDec
    let DataD ddCxt ddName ddTyVarBndrs ddMbyKind ddCons ddDerivClauses = typeDec
    let constrs = ddCons
    ListE <$> createCons ddCons

createCons :: [Con] -> Q [Exp]
createCons cons = mapM con2exp cons

con2exp :: Con -> ExpQ
con2exp (NormalC name btys) = do
    exps <- mapM bt2arg btys
    return $ AppE (ConE name) (apply exps)
  where
    apply :: [Exp] -> Exp
    apply [e] = e
    apply (e : es) = AppE e (apply es)


bt2arg :: BangType -> ExpQ
bt2arg (bang, ty) =
    case ty of
        AppT (ConT name) t2
            | name == ''Xp.Core.Xp -> [e| (Xp.Core.SVar) |]
        _ -> error $ "Non-Xp argument to data constructor: " <> show ty


