{-# LANGUAGE MultiParamTypeClasses   #-}

module E.Dummies where


{- | Types that implement an instances of the 'Dummies' class provide
default values for all constructors of their type. The type must also
implement 'Generics.SOP.Generic', as we plan to use the SOP representation
of these types. We do __not__ use the values stored in the fields of the
constructors, so those can be set to @undefined@. The 'deriveDummies' function
in 'E.TH' does this for its genereated values.
-}
class Dummies a where
    mkDummies :: [a]
