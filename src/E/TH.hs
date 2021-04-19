{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}


module E.TH where

import Language.Haskell.TH

import qualified Data.List as List

import qualified E.Dummies


mkConstructors :: Name -> Q [Dec]
mkConstructors victim = do
    [d|

        |]

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
