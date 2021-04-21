# Description

This repository implements a technique for embedding pattern matching into a
Haskell EDSL, with the intention of integrating the ideas into the
[Haski](https://github.com/OctopiChalmers/haski) compiler.

Specifically, a framework is provided that allows the user to:

1. Define how to convert some input type (e.g. `Int`) into a user-defined ADT.

2. Use the above definition to convert into, and deconstruct using pattern
matching, values of the user-defined ADT.

The technique is demonstrated in a simple expression language `E` along with
a compiler targeting C.

Example programs can be found in the `src/E/Examples` directory.

## Building

To build the project, run `cabal build` using a version of `cabal-install`
2.4 or newer. The `.cabal` file specifies a `base ^>= 4.14.1.0` dependency,
corresponding to GHC version >= 8.10.2, but things probably work equally well
with older versions of GHC, so relaxing the requirements should work fine.

To also generate Haddock documentation for the project as an HTML file, run
`cabal haddock`.

# The expression language

The expression language `E` is a straightforward deep embedding, supporting
simple arithmetics and some basic bitwise operations. The constructors are
given by the GADT found in [E.Core](src/E/Core.hs). Functions for compiling
`E` programs can be found in [E.Compile](src/E/Compile.hs). The generated C
does not handle input; its `main()` function simply runs with a hardcoded
value. `E` is intended to represent a generic embedded language, so that
techniques that work for `E` should also work for other embedded languages.

# A worked example

This section will walk through the hypothetical example use case presented
in [E.Examples.Watering](src/E/Examples/Watering.hs). The example is chosen
with the target domain of Haski in mind (IoT-devices).

---

An example of a typical smart device is a sensor which can collect data from
its environment, and send the data to some application which can process it.
Imagine that we are growing plants, and that we want to hook up a sensor to
collect some information about the air temperature and soil humidity, so that
a connected application can determine whether the plants need watering.

For low-level devices such as this, it is common for the sent data to be
encoded in a format that might be a bit inconvenient to work with. For this
example, imagine the data is encoded within a 16-bit integer, and that it can
take one of two forms:

__If control bit C is set:__
```
| C = 1 | 7           | 8             |
  ^       ^             ^------------ Temperature (degrees Celsius)
  |       +-------------------------- Humidity (percentage)
  +---------------------------------- Control bit
```

__If control bit C is not set:__
```
| C = 0 | 15                          |
  ^        ^------------------------- Error code
  +---------------------------------- Control bit
```

The data is encoded within the 16-bit integer, but has two intepretations
(given by the specification of the sensor). Which one to use depends on
whether the most significant bit, the _control_ bit, is set:

* If the control bit is set, then the first 8 bits of the input represent the
temperature in degrees Celsius as a signed 8-bit integer. The following 7
bits represent the humidity percentage of the soil as a 7-bit unsigned
integer (a bit of a nonsensical representation to be fair, as it only spans
the 1% - 99% range).

* If the control bit is _not_ set, then the sensor is signals an error, and the
remaining 15 bits represent some sort of error code.

Now, say we wanted to perform some logic based on the input sensor data. For
example, we might want a function to return `True` if the soil humidity is
sufficiently low, or if the temperature is sufficiently high.

The type signature for such a function would look something like this:

```haskell
needsWatering :: E Word16 -> E Bool
```

The problem is that we need to perform a bunch of bitwise masking and
shifting to extract the data we want, and things could clutter up the
function body quite a bit, which might obscure our application logic.
Besides, the form of the sensor data will not change while the program runs;
it is always going to be one of the two encodings.

Therefore, we should model the two encodings of the sensor data as a Haskell
data type:

```haskell
type Temp      = E Word8   -- Unsigned 8-bit integer
type Humidity  = E Int8    -- Signed 8-bit integer
type ErrorCode = E Word16  -- Unsgined 16-bit integer

data SensorData
    = Sensor Temp Humidity
    | Error ErrorCode

toSensorData :: E Word16 -> SensorData
{- ... -}
```

Our data type `SensorData` has two constructors, corresponding to the two
different encodings for our sensor data. The fields of the constructors
correspond to the encoded data; the integer types used are the smallest sizes
of multiples of 8 that can fit the data. We could also define a `toSensorData`
conversion function to contain most of the dirty bitwise operations, so that
the application logic stays clean.

Now that we have our custom data type, which is more convenient to work with
than the 16-bit binary encoding, we'd like to define `needsWatering` similar
to this:

```haskell
needsWatering :: E Word16 -> E Bool
needsWatering x = case toSensorData x of
    Sensor temp humidity -> temp >. 30 ||. humidity <. 25
    Error _errorCode     -> valE False  -- Placeholder; do something sensible here
```

The operators `>.`, `||.`, and `<.` are variants of the normal `>`, `||`, and
`<`, but operating on values in the expression language. `valE :: a -> E a`
simply brings a value into the expression languge.

The problem with an implementation like this is that our pattern matching in
the `case`-expression is not on the embedded level. When we _compile_
`needsWatering`, the result of `toSensorData x` will be _either_ a `Sensor`
value or an `Error` value. We want to inspect the value of `toSensorData x`
during the runtime of the generated program in our _target_ language (C), but
currently we instead inspect it during the runtime of our _host_ language
(Haskell). What we need is a _representation_ of the `case`-expression inside
the expression language, as a constructor of the `E` type.

## `Partition` and `match`



