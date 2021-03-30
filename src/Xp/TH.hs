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
            _ ->  error $ "deriveStruct: Invalid definition of type `"
                <> show victim <> "`"

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
            _ ->  error $ "deriveStruct: Invalid definition of type `"
                <> show victim <> "`"

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
