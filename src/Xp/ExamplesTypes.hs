{- | This module serves solely to contain data types used for partitioning.

In other words, every type in this module exists for the purpose of creating
Partition instances in another module. This structure is entirely due to TH
limitations; we cannot create expressions with splicing on types defined in
the same module as the splice is used. This means that, because the splice is
intended to be used as the body of @Partition@ class method @concstructors@,
the data types for which the instance declaration is written for must be
__imported__.

This is pretty ugly as it is unintuitive and results in orphan instances.
Ideally, we'd like to automate and generalize the call to the
@makeConstructors@ function also, but that seems not possible with TH.

-}

module Xp.ExamplesTypes where

import Xp.Core
