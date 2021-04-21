{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Types with supported representation in C.

module E.CTypes where

import Data.Char (toLower)
import Data.Int (Int8)
import Data.Word (Word8, Word16)


class CType a where
    ctype :: String
    cval :: a -> String
    cformat :: Char

instance CType Int where
    ctype = "int"
    cval = show
    cformat = 'd'

-- Uses <stdint.h>
instance CType Int8 where
    ctype = "int8_t"
    cval = show
    cformat = 'd'

-- Uses <stdint.h>
instance CType Word8 where
    ctype = "uint8_t"
    cval = show
    cformat = 'd'

-- Uses <stdint.h>
instance CType Word16 where
    ctype = "uint16_t"
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
