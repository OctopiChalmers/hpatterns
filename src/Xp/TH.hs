{- Functions are going to be super-partial, but it's fine (sort of) for
TH functions since they will raise compile-time errors anyway. Still, it
would be better to provide nicer error messages using `error` where
appropriate.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE TemplateHaskell #-}

module Xp.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.List as List

import qualified Xp.Core



{- | Enumerate the constructors of some type, inserting 'SVar' wherever
something of type (Xp a) is required.

For example, given the following definition:

> data Sig a = Pos (Xp a) | Neg (Xp a) | Zero

@makeConstructors ''Sig@ will generate the following:

> [Pos SVar, Neg SVar, Zero]
-}
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
    bt2arg (_bang, ty) =
        case ty of
            AppT (ConT name) _t2
                | name == ''Xp.Core.Xp -> [e| (Xp.Core.SVar) |]
            _ -> error $ "Non-Xp argument to data constructor: " <> show ty

deriveStruct :: Name -> Q [Dec]
deriveStruct victim = do
    TyConI d <- reify victim

    let myTypeNameQ = conT victim
    let myStructName = getStructName d
    let myDummyQ = getDummy d
    (myToFieldsPat, myToFieldsExp) <- getToFields d
    (myFromFieldsPat, myFromFieldsExp) <- getFromFields d

    [d|
        instance Xp.Core.Struct $(myTypeNameQ) where
            structName = myStructName
            dummy = $(myDummyQ)
            toFields $(return myToFieldsPat) = $(return myToFieldsExp)
            fromFields $(return myFromFieldsPat) = $(return myFromFieldsExp)
        |]

  where
    getFromFields :: Dec -> Q (Pat, Exp)
    getFromFields (DataD _ _ _ _ [NormalC cName bTypes] _) = do
        let numArgs = length bTypes
        let names = take numArgs nameList

        let exp = apply (ConE cName) $ map VarE names

        let typesNames = zip (map snd bTypes) names
        pats <- mapM genFieldPat typesNames

        return (ListP pats, exp)

    genFieldPat :: (Type, Name) -> Q Pat
    genFieldPat (t, n@(Name (OccName s) _)) =
        [p| Xp.Core.Field $(tCon) $(litP $ stringL s) $(varP n) |]
      where
        tCon :: Q Pat
        tCon = case t of
            AppT (ConT xpTy) (ConT innerTy)
                | xpTy == ''Xp.Core.Xp && innerTy == ''Int    -> [p| Xp.Core.TInt |]
                | xpTy == ''Xp.Core.Xp && innerTy == ''Double -> [p| Xp.Core.TDouble |]
                | xpTy == ''Xp.Core.Xp && innerTy == ''Bool   -> [p| Xp.Core.TBool |]

    getToFields :: Dec -> Q (Pat, Exp)
    getToFields (DataD _ _ _ _ [NormalC cName bTypes] _) = do
        let numArgs = length bTypes
        let names = take numArgs nameList

        let pat = ConP cName $ map VarP names

        let typesNames = zip (map snd bTypes) names
        fields <- mapM genFieldExp typesNames

        return (pat, ListE fields)

    genFieldExp :: (Type, Name) -> Q Exp
    genFieldExp (t, n@(Name (OccName s) _)) = [| Xp.Core.Field $(tCon) s $(varE n) |]
      where
        tCon :: Q Exp
        tCon = case t of
            AppT (ConT xpTy) (ConT innerTy)
                | xpTy == ''Xp.Core.Xp && innerTy == ''Int    -> [| Xp.Core.TInt |]
                | xpTy == ''Xp.Core.Xp && innerTy == ''Double -> [| Xp.Core.TDouble |]
                | xpTy == ''Xp.Core.Xp && innerTy == ''Bool   -> [| Xp.Core.TBool |]

    getStructName :: Dec -> String
    getStructName (DataD _ (Name (OccName s) _) _ _ _ _) = s

    getDummy :: Dec -> Q Exp
    getDummy (DataD _ _ _ _ [NormalC cName bTypes] _) = do
        let numArgs = length bTypes
        arg <- [| error "DUMMY ARG" |]
        return $ apply (ConE cName) (replicate numArgs arg)
      where

    consName :: Dec -> Name
    consName (DataD _ _ _ _ [NormalC cName _] _) = cName

apply :: Exp -> [Exp] -> Exp
apply e = List.foldl' AppE e

-- TODO: We can run out of names in theory like this
nameList :: [Name]
nameList = map mkName $ map (: []) ['a' .. 'z']
