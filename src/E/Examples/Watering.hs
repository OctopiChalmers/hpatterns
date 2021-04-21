{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module E.Examples.Watering where

import Data.Int (Int8)
import Data.Word (Word8, Word16)

import E.Core
import E.TH (mkConstructors)


{- Example: sensor data encoded as an unsigned 16 bit integer:

If control bit C is set:
| C = 1 | 7           | 8             |
  ^       ^             ^------------ Temperature (degrees Celsius)
  |       +-------------------------- Humidity (percentage)
  +---------------------------------- Control bit

If control bit C is not set:
| C = 0 | 15                          |
  ^        ^------------------------- Error code
  +---------------------------------- Control bit

-}

type Temp      = E Word8
type Humidity  = E Int8
type ErrorCode = E Word16

data SensorData
    = Sensor Temp Humidity
    | Error ErrorCode
$(mkConstructors ''SensorData)

needsWatering :: E Word16 -> Estate (E Bool)
needsWatering x = match x $ \case
    Sensor temp humidity -> temp >. 30 ||. humidity <. 25
    Error _errorCode     -> valE False  -- Do something sensible here

instance Partition SensorData Word16 where
    partition :: [E Word16 -> (E Bool, Estate SensorData)]
    partition =
        [ \ v -> ( testBitE 15 v
                 , _Sensor (castE (v &. tempMask))
                           (castE (v &. humidityMask >>. 8))
                 )
        , \ v -> ( notE (testBitE 15 v)
                 , _Error (v &. errorMask)
                 )
        ]
      where
        tempMask :: E Word16
        tempMask = 0b0000_0000_1111_1111

        humidityMask :: E Word16
        humidityMask = 0b0111_1111_0000_0000

        errorMask :: E Word16
        errorMask = 0b0111_1111_1111_1111

{- | Smart constructors for the partition type.

These can be generated
automatically by the TH function 'mkConstructors'. The ones below have double
underscores to avoid name collision.

> _Sensor :: Temp -> Humidity -> Estate SensorData
> _Sensor a b = do
>     t1 <- newFieldTag
>     t2 <- newFieldTag
>     pure $ Sensor (t1 a) (t2 b)

> __Error :: ErrorCode -> Estate SensorData
> __Error a = do
>     t1 <- newFieldTag
>     pure $ Error (t1 a)

An alternative is to use pattern synonyms for smart constructors; this
looks nicer (closer to the "real thing" in terms of syntax highlighting
etc.), but has the issue that we must define some deconstruction (pattern
matching) behavior, even if we only need the construction aspect.

> pattern Sensor_ :: Temp -> Humidity -> Estate SensorData
> pattern Sensor_ x y <- (error (errMsg "Sensor_") -> (x, y))
>   where
>     Sensor_ a b = do
>         t1 <- newFieldTag
>         t2 <- newFieldTag
>         pure $ Sensor (t1 a) (t2 b)

> pattern Error_ :: ErrorCode -> Estate SensorData
> pattern Error_ x <- (error (errMsg "Error_") -> x)
>   where
>     Error_ a = do
>         t1 <- newFieldTag
>         pure $ Error (t1 a)

> errMsg :: String -> String
> errMsg name = concat
>     [ "Pattern synonym `", name, "` should only be used for construction,"
>     , " not for destructuring."
>     ]

-}
