# Description

This repository implements a technique for embedding pattern matching into a
Haskell EDSL, with the intention of integrating the ideas into the
[Haski](https://github.com/OctopiChalmers/haski) compiler.

__EDIT (2021-06-25):__ Precise implementation details in this repository may
differ slightly from that of the Haski compiler. In general though, the
technique used is the same.

Specifically, a framework is provided that allows the user to:

1. Define how to convert some input type (e.g. `Int`) into a user-defined ADT.

2. Use the above definition to convert into, and deconstruct using pattern
matching, values of the user-defined ADT.

The technique is demonstrated in a simple expression language `E` along with
a compiler targeting C.

Example programs can be found in the `src/E/Examples` directory.

## Building

The library can be built with [Stack](https://www.haskellstack.org):

```sh
stack build
```

To generate documentation:

```sh
stack haddock
```

To compile one of the example programs, first run GHCi:

```sh
stack repl
```

Then, apply the `printProg` function to an `E` expression:

```sh
# A dummy argument may need to be provided to the program; the compiled
# programs don't read arguments.
GHCi> printProg (needsWatering 0)
```

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

## Input data

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

## Writing a program

Now, say we wanted to perform some logic based on the input sensor data. For
example, we might want a function to return `True` if the soil humidity is
sufficiently low, or if the temperature is sufficiently high.

The type signature for such a function could look something like this:

```haskell
needsWater :: E Word16 -> E Bool
```

The problem is that we need to perform a bunch of bitwise masking and
shifting to extract the data we want, and things could clutter up the
function body quite a bit, which might obscure our application logic.
Besides, the form of the sensor data will not change while the program runs;
it is always going to be one of the two encodings.

Therefore, we might want to define a Haskell data type to model the two
encodings of the sensor data.

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
correspond to the encoded data; the integer types used (`Int8`, `Word8`,
`Word16`) are the smallest sizes of multiples of 8 that can fit the data. We
could now define a `toSensorData` conversion function to contain most of the
dirty bitwise operations, so that the application logic stays clean.

Now that we have our custom data type, which is more convenient to work with
than the 16-bit binary encoding, we'd like to define `needsWater` similar
to this:

```haskell
needsWater :: E Word16 -> E Bool
needsWater x = case toSensorData x of
    Sensor temp humidity -> temp >. 30 ||. humidity <. 25
    Error _errorCode     -> valE False  -- Placeholder; do something sensible here
```

The operators `>.`, `||.`, and `<.` are variants of the normal `>`, `||`, and
`<`, but operating on values in the expression language. `valE :: a -> E a`
simply brings a value into the expression languge.

The problem with an implementation like this is that our pattern matching in
the `case`-expression is not on the embedded level, like our temperature- and
humidity-checking logic is. When we _compile_ `needsWater`, the result of
`toSensorData x` will be _either_ a `Sensor` value or an `Error` value. What
we want is to inspect the value of `toSensorData x` during the runtime of the
generated _target_ language program (C), but currently we instead inspect it
during the runtime of our _host_ language program (Haskell). What we need is
a _representation_ of the `case`-expression inside the expression language,
as a constructor of the `E` type.

## `Partition` and `match`

To work around this problem, we use the framework provided and modify our
`needsWater` program to the following:

```haskell
{-# LANGUAGE LambdaCase #-}

needsWatering :: E Word16 -> Estate (E Bool)
needsWatering x = match x $ \case
    Sensor temp humidity -> temp >. 30 ||. humidity <. 25
    Error _errorCode     -> valE False  -- Do something sensible here
```

We note that we call a function `match` in place of `toSensorData` and that
we no longer use the built-in Haskell `case of`-construct, instead
providing a function defined using a lambda-case (enabled by the
[`LambdaCase`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html)
languge pragma). The return type is also slightly different; `Estate` is
simply a specialization of the `State` monad and is used to generate unique
identifiers when compiling the `E` AST:

```haskell
type Estate = State Env
data Env = {- ... -}
```

To understand how this works, we first take a look at the signature for the
`match` function, here slightly simplified:

```haskell
match :: Partition a p => E a -> (p -> E b) -> Estate (E b)
```

`match` takes value of type `E a` as its first argument, this is the
_scrutinee_. The second argument is the function that can perform pattern
matching, but it takes a value of type `p`, not `E a`. When `match` is
applied, it will transform the `E a` value to a `p` value; this
transformation is indicated by the `Partition a p` constraint and
corresponds to the `toSensorData` function from the earlier example.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

class Partition a p where
    partition :: [E a -> (E Bool, p)]
```

A `Partition a p` instance defines how to transform values from some input
type `a` to a _sum_ type `p`. Since normal Haskell `data`-style types are sum
types, this means that we can define instances of `Partition` to transform
values of other types into instances of our user-defined types, such as
`SensorData`. Because sum types can have multiple constructors, an instance
of `Partition a p` must also define _conditions_ to determine which
constructor to use, depending on the value of the input (of type `a`).

Note that there are no actual constraints on the type variables `a` and `p`;
they can be any types, in theory. However, seeing as the point of `Partition` is to
define transformation and construction of data, it might be hard though to
implement anything useful for uninhabited types (such as `Void`), for example.

`partition :: [E a -> (E Bool, Estate p)]` takes no arguments and returns a
list of functions where each function represents one constructor of the sum
type `p`, or one "branch" of a `case of`-construct. Each function takes a
value of the input type `a` and returns a pair:

* The first half of the pair is a _predicate_ on the input value and affects
program control-flow in the target language; the `E Bool` expression
corresponds to the condition in the generated if-statement.

* The second half of the pair constructs a value of the sum type `p` which can
be dependent on the input value. This provides the functionality of
`toSensorData` from earlier.

The following would be a possible definition of a `Partition` instance for
our example use case:

```haskell
{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}

instance Partition Word16 SensorData where
    partition =
        [ \ v -> ( testBitE 15 v
                 , Sensor (castE (v &. tempMask))
                          (castE (v &. humidityMask >>. 8))
                 )
        , \ v -> ( notE (testBitE 15 v)
                 , Error (v &. errorMask)
                 )
        ]
      where
        -- Language extensions BinaryLiterals and NumericUnderscores enables
        -- nicer literals for binary values.
        tempMask, humidityMask, errorMask :: E Word16
        tempMask     = 0b0000_0000_1111_1111
        humidityMask = 0b0111_1111_0000_0000
        errorMask    = 0b0111_1111_1111_1111
```

In each of the corresponding `E Bool` fields in our pairs, we implement the
check for the control bit in our input data, and apply constructors `Sensor`
and `Error` in the second half of the pair accordingly. `testBitE` checks
whether a given bit is set, and `castE` and `>>.` are straightforward
C translations of type-casting and bitwise shift-right operations.

When constructing our `needsWatering` program, `match` will apply all the
functions given by `partition` to a placeholder value `ESym`, thus creating
values of `SensorData` using both our constructors:

```haskell
-- Simplified snippet from the definition of 'match`
let branches = map ($ ESym) $ partition @a @p
```

Then, when generating the C code, the compiler can figure out how to replace
the `ESym` values with that of the scrutinee.

Below is the corresponding C output (slightly edited for clarity) for the
pattern match and `match` usage in `needsWatering`:

```C
uint16_t _scrut1;

bool v0(uint16_t arg) {
    _scrut1 = arg;
    bool res;

    if (((1 << 15) & _scrut1) != 0) {
        res = ((int8_t) (_scrut1 & 255)) > 30 || ((uint8_t) ((_scrut1 & 32512) >> 8)) < 25;
    } else if (!(((1 << 15) & _scrut1) != 0)) {
        res = false;
    } else {
        fprintf(stderr, "No match on: `_scrut1`\n");
        exit(1);
    }
    return res;
}
```

We can see that the right-hand sides of the lambda-case function from
`needsWatering` has been applied in some form on the scrutinee, in combination
with whatever transformation was applied by `partition`.

On a quick note, one might wonder why the variable `_scrut` is necessary,
seeing as it immediately gets assigned the function argument `arg`. Why not
use `arg` directly? The reason is only an implementation convenience to avoid
scoping errors that can otherwise occur when nestling calls to `match`. The
current compiler creates single-parameter function defintions to handle each
call to `match`. In the generated C code, this means that a variable from an
outer `match` is invisible inside the function body generated from an
inner match, even though this differs from the Haskell scoping:

```haskell
f :: E Int -> Estate (E Bool)
f x = matchM x $ \case
    A2 b n -> b
    A1 n   -> match n $ \case
        A1 _   -> b  -- <- b refers to a field in the outer match.
        A2 t _ -> t
```

## Lessen duplicate computation

Our `needsWatering` example looks fairly OK at this point, but there exists a
potential issue related to our technique of pre-applying both `partition` and
the lambda function on which `match` is applied. The issue manifests when
fields of constructors are used _multiple times_ in the body of the `match`
call.

Consider a different (contrived) example, and the generated C code (again,
edited for clarity):

```haskell
contrived :: E Int -> Estate (E Bool)
contrived v = match v $ \case
    T1 n -> n + n >. n
    T2   -> valE False

data T = T1 (E Int) | T2

instance Partition Int T where
    partition =
        [ \ v -> (v >=. 0, T1 (v * 10 - 2 * 10 - 2))
        , \ v -> (v <.  0, T2)
        ]
```
```c
int _scrut1;

bool v0(int arg) {
    _scrut1 = arg;
    bool res;

    if (_scrut1 >= 0) {
        // Duplicate computations!
        res = (((((_scrut1 * 10) - (2 * 10)) - 2)
               + (((_scrut1 * 10) - (2 * 10)) - 2))
               > (((_scrut1 * 10) - (2 * 10)) - 2));
    } else if (_scrut1 < 0) {
        res = false;
    } else {
        fprintf(stderr, "No match on: `_scrut1`\n");
        exit(1);
    }

    return res;
}
```

In the body of the `match` call, we only performed two operations, `+` and
`>.`. Yet, because we reference the expression `n` multiple times, its entire
definition (as defined by `partition`) shows up multiple times as well! This
is a problem in several aspects. For one, it clutters the code and
highlights the lack of a relation between the fields of our constructors and
corresponding variables in the generated code. More importantly, however, it
leads to extra, unnecessary computation in the generated code.

What we would like is for each field of our constructors to be assigned to a
variable in the generated code, so that they can be reused. For this to
happen, the compiler needs to know when an expression corresponds to a field
of a constructor, for example by having expressions tagged with some kind of
dedicated "tag" constructor. Implementing this is a bit tricky though; the
`match` function itself does not know enough about the type of the value it
operates on to meaningfully inspect each constructor field, though it might
be possible through generic programming techniques such as GHC generics or
similar.

Instead, the simplest way would be to apply a tag constructor `EField` in the
defintion of `partition`, where the value is constructed:

```haskell
EField :: ArgId -> E a -> E a
-- Simplified type of ArgId to demonstrate the idea; the actual type is
-- slightly different, but conceptually the same.
type ArgId = String
```
```haskell
instance Partition Int T where
    partition =
        [ \ v -> (v >=. 0, T1 (EField "T1_1" (v * 10 - 2 * 10 - 2)))
        , \ v -> (v <.  0, T2)  -- ^ -- Tag constructor
        ]
```

`EField` wraps an expression with a tag, indicating to the compiler that it
should assign the entire sub-expression to a variable, and use that variable
in the future if the expression is referenced again. This works, but it has
the downside of introducing additional boilerplate to the user, which is both
unergonomic and potentially error-prone, especially so for larger types and
for constructor with more fields. The `EField` tag only serves its purpose
internally anyway, so it should be ideally be hidden away from the user.

Here is where we can use _smart constructors_ to hide the tagging of field
expressions. A smart constructor is a normal function that wraps the
construction of a value, possibly performing additional work that can be
hidden away from the user.

With a smart constructor, we could instead implement `partition` like this:

```haskell
instance Partition Int T where
    partition =
        [ \ v -> (v >=. 0, _T1 (v * 10 - 2 * 10 - 2))
        , \ v -> (v <.  0, T2)
        ]

-- Smart constructor for constructor T1.
_T1 :: E Int -> T
_T1 a = let t1 = EField "T1_1"
        in T1 (t1 a)
```

Now the implementation of `partition` is minimally impacted, as we switch
from writing `T1` to `_T1`. Of course, we now instead have an entire separate
smart constructor function that needs to be written instead, so we have only
really moved the problem elsewhere.

Thankfully, these smart constructors follow a pattern, and the smart
constructor name, type, number of parameters, and number tags to create, can
all be derived from the type definition of value that it constructs. While
the tagging would be difficult to do in `match` due to operating on a
polymorphc type, we _can_ generate our smart constructors using Template
Haskell.

With this, we can add a single line to our data type declaration to generate
smart constructors for it:

```haskell
{-# LANGUAGE TemplateHaskell #-}

data T = T1 (E Int) | T2
$(mkConstructors ''T)

instance Partition Int T where
    partition =
        [ \ v -> (v >=. 0, _T1 (v * 10 - 2 * 10 - 2))
        , \ v -> (v <.  0, _T2)  -- Not necessary (0 fields) but looks uniform :)
        ]
```

As a small detail, the generated code from the TH function is slightly
different from our hand-written one:

```haskell
_T1 :: E Int -> Estate T
_T1 v0 = do
    tag1 <- newFieldTag
    pure $ T1 (tag1 a)
```

`newFieldTag` generates a unique tag name, which will be unique in the entire
program. This results in the smart constructor having the return type
`Estate a` instead of simply `a`, which also changes the definition of
`partition` accordingly:

```haskell
partition :: [E a -> (E Bool, Estate p)]
--                            ^^^^^^
```

Using the smart constructors, our generated code now looks like this instead:

```c
int _scrut1;
int _scrut1_field1;

bool v0(int arg) {
    _scrut1 = arg;
    bool res;
    _scrut1_field1 = ((_scrut1 * 10) - (2 * 10)) - 2;

    if (_scrut1 >= 0) {
        res = (_scrut1_field1 * _scrut1_field1) > _scrut1_field1;
    } else if (_scrut1 < 0) {
        res = false;
    } else {
        fprintf(stderr, "No match on: `_scrut1`\n");
        exit(1);
    }
    return res;
}
```

The duplicate computations from earlier are gone and the relation to the
original `E` program is a bit clearer as well!
