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
value.

# A worked example

This section will walk through the hypothetical example use case presented
in [E.Examples.Watering](src/E/Examples/Watering.hs).
