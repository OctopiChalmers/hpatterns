{-# LANGUAGE AllowAmbiguousTypes #-}

module E.CTypes where

import Data.Char (toLower)


class CType a where
    ctype :: String
    cval :: a -> String
    cformat :: Char

instance CType Int where
    ctype = "int"
    cval = show
    cformat = 'd'

instance CType Double where
    ctype = "double"
    cval = show
    cformat = 'f'

instance CType Bool where
    ctype = "bool"
    cval = map toLower . show
    cformat = 'd'
