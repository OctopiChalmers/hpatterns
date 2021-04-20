{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module E.TH where

import Language.Haskell.TH

import qualified Data.List as List

import qualified E.Core
import qualified E.Dummies


mkConstructors :: Name -> Q [Dec]
mkConstructors victim = do
    TyConI d <- reify victim
    genConstructors d
  where
    genConstructors :: Dec -> Q [Dec]
    genConstructors (DataD [] _tName [] _ constructors _) = mapM genCon constructors
    genConstructors _ = err $ concat
        [ "Invalid data declaration for type `", show victim, "`. Currently"
        , " only allows very simple data declarations, e.g. no support for"
        , " type variables."
        ]

    genCon :: Con -> Q Dec
    genCon (NormalC cName (map snd -> types)) = do
        let fName = mkName ('_' : nameBase cName)
        fClause <- genClause cName types
        pure $ FunD fName [fClause]

    genCon _ = err $ concat
        [ "Invalid constructor form in declaration of data type ` "
        , show victim, "`. Only 'normal' constructors are allowed;"
        , " no record syntax, no existential quantification etc."
        ]

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

    err :: String -> a
    err s = error $ "Error in TH function `mkConstructors`: " ++ s


{- | Create an instance declaration of the 'E.Dummies.Dummies' class.
The generated definition of 'E.Dummies.mkDummies' sets all arguments
of constructors to undefined (or error, really).
-}
deriveDummies :: Name -> Q [Dec]
deriveDummies victim = do
    TyConI d <- reify victim

    let victimDummiesQ = genDummies d

    [d|
        instance E.Dummies.Dummies $(conT victim) where
            mkDummies = $(victimDummiesQ)
        |]
  where
    genDummies :: Dec -> Q Exp
    genDummies = \case
        DataD _ _ _ _ constructors _ -> ListE <$> mapM genDummy constructors

        _ -> err $ concat ["`", show victim, "` is not a data declaration."]

    genDummy :: Con -> Q Exp
    genDummy = \case
        NormalC cName bTypes -> do
            let numArgs = length bTypes
            arg <- [| error "DUMMY ARG" |]
            return $ (List.foldl' AppE) (ConE cName) (replicate numArgs arg)

        _ -> err $ concat
            [ "Invalid constructor form in declaration of data type ` "
            , show victim, "`. Only 'normal' constructors are allowed, "
            , "e.g. no record syntax or existential quantification."
            ]

    err :: String -> a
    err s = error $ "Error in TH function `deriveDummies`: " ++ s
