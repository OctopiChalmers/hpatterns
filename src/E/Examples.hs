{- | Generate code with 'printProg' from 'E.Compile'. -}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module E.Examples where

import Generics.SOP  -- for now

import E.Core

import qualified GHC.Generics as GG (Generic)


{- | Ex 0. Simplest variant, does not require SOP, but makes for more
verbose partition instances.
-}
ex0 :: E Double -> Estate (E Int)
ex0 v = match2 v $ \case
    T1 n -> n + 1
    T2   -> 0

data T = T1 (E Int) | T2
    deriving (GG.Generic, Generic)

instance Partition T Double where
    partition =
        [ \ v -> (v >. 0, T1 (floorIntE v))
        , \ v -> (v <. 0, T2)
        ]

ex1 :: E Double -> Estate (E Int)
ex1 v = match @Sig v $ \case
    SOP    (Z (I n :* Nil)) -> n + 2
    SOP (S (Z Nil        )) -> 0
    SOP (S (S _          )) -> error "impossible by construction"

data Sig = Pos (E Int) | Neg
    deriving (GG.Generic, Generic)

instance Partition Sig Double where
    partition =
        [ \ v -> (v >. 0, Pos $ floorIntE v)
        , \ v -> (v <. 0, Neg)
        ]

ex2 :: E Double -> Estate (E Int)
ex2 v = matchM @Sig v $ \case
    SOP (S (S _          )) -> error "impossible by construction"
    SOP (S (Z Nil        )) -> pure 0
    SOP    (Z (I n :* Nil)) -> matchM @Size n $ \case
        SOP    (Z (I x :* I y :* Nil)) -> pure $ x + y
        SOP (S (Z (I x :* Nil)      )) -> pure $ x + n
        SOP (S (S _                 )) -> error "impossible by construction"

data Size
    = Large (E Int) (E Int)
    | Small (E Int)
    deriving (Generic, GG.Generic)

instance Partition Size Int where
    partition =
        [ \ v -> (v >. 100, Large v v)
        , \ v -> (valE True, Small (v + 1))
        ]

{- | Ex 3. Using pattern synonyms, we can improve ergonomics. -}

pattern Pos_ :: E Int -> Rep Sig
pattern Pos_ n = SOP (Z (I n :* Nil))
pattern Neg_ :: Rep Sig
pattern Neg_ = SOP (S (Z Nil))

pattern Large_ :: E Int -> E Int -> Rep Size
pattern Large_ x y = SOP (Z (I x :* I y :* Nil))
pattern Small_ :: E Int -> Rep Size
pattern Small_ x = SOP (S (Z (I x :* Nil)))

ex2improved :: E Double -> Estate (E Int)
ex2improved v = matchM @Sig v $ \case
    Neg_   -> pure 0
    Pos_ n -> matchM @Size n $ \case
        Large_ x y -> pure (x + y)
        Small_ x   -> pure (x + n)



    -- Non-exhaustive without this???
        _ -> undefined
    _ -> undefined

