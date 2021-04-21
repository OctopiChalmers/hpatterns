{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module E.TH where

import Language.Haskell.TH

import qualified Data.List as List

import qualified E.Core


{- | Create smart constructors for some partition type.

For example, for some type T:

> data T
>     = T1 (E Bool) (E Int)
>     | T2 (E Int)

Generates the following smart constructors:

> _T1 :: E Bool -> E Int -> Estate T
> _T1 v0 v1 = do
>     tag0 <- newFieldTag
>     tag1 <- newFieldTag
>     pure $ T1 (tag0 v0) (tag1 v1)
>
> _T2 :: E Int -> Estate T
> _T2 v0 = do
>     tag0 <- newFieldTag
>     pure $ T2 (tag0 v0)

-}
mkConstructors :: Name -> Q [Dec]
mkConstructors victim = do
    TyConI d <- reify victim
    genConstructors d
  where
    genConstructors :: Dec -> Q [Dec]
    genConstructors (DataD [] tName [] _ constructors _) = do
        funs <- mapM genCon constructors
        sigs <- mapM (genSig (ConT tName)) constructors
        pure $ funs ++ sigs
    genConstructors _ = err $ concat
        [ "Invalid data declaration for type `", show victim, "`. Currently"
        , " only allows very simple data declarations, e.g. no support for"
        , " type variables."
        ]

    genSig :: Type -> Con -> Q Dec
    genSig retType (NormalC cName (map snd -> types)) = do
        let fName = mkName ('_' : nameBase cName)
        let retTypeM = AppT (ConT ''E.Core.Estate) retType
        let typeSig = List.foldr1 (AppT . AppT ArrowT) $ types ++ [retTypeM]
        pure $ SigD fName typeSig
    genSig _ _ = invalidCon

    genCon :: Con -> Q Dec
    genCon (NormalC cName (map snd -> types)) = do
        let fName = mkName ('_' : nameBase cName)
        fClause <- genClause cName types
        pure $ FunD fName [fClause]
    genCon _ = invalidCon

    genClause :: Name -> [Type] -> Q Clause
    genClause cName types = do
        let numArgs = length types
        let argNames = take numArgs (infVarNames "v")
        let pats = map VarP argNames
        body <- genBody argNames
        pure $ Clause pats body []
      where
        genBody :: [Name] -> Q Body
        genBody argNames = do
            e <- [e| E.Core.newFieldTag |]
            let tagNames = take (length argNames) (infVarNames "tag")
            let bindings = map (flip BindS e) (map VarP tagNames)
            -- bindings:
            -- v0 <- newFieldTag
            -- v1 <- newFieldTag
            -- ...
            let conArgs = zipWith (\ t a -> AppE (VarE t) (VarE a)) tagNames argNames
            let construction = List.foldl' AppE (ConE cName) conArgs
            returnStmt <- NoBindS <$> [e| return $(pure construction) |]
            -- returnStmt:
            -- pure $ SomeConstructor (t0 v0) (t0 v1) ...

            pure . NormalB . DoE $ bindings ++ [returnStmt]

    infVarNames :: String -> [Name]
    infVarNames prefix = map (mkName . (prefix ++) . show) ([0 ..] :: [Int])

    invalidCon :: a
    invalidCon = err $ concat
        [ "Invalid constructor form in declaration of data type ` "
        , show victim, "`. Only 'normal' constructors are allowed;"
        , " no record syntax, no existential quantification etc."
        ]

    err :: String -> a
    err s = error $ "Error in TH function `mkConstructors`: " ++ s
