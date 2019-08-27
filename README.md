# hasktorch-naperian

This README and further docs are in progress.

## Goal

To provide a finite Naperian functor based interface to `hasktorch`.

## Install and build

To build, clone Hasktorch `0.2.0.0` into the same parent directory and build.
Then run `stack build` in the top level of this project.
The resulting binary requires some shared objects.
One way to run the binary is to copy it to the top-level `hasktorch` directory
and execute `source setenv` while in `bash`.

## Status

The official Google Summer of Code project has ended.

### Finite Naperian functors

The module `Data.Naperian` contains:

- Instances of `Enum` and `Bounded` for `(a, b)` and `Either a b`.
- `Naperian` instances for `Product` and `Compose`.
- A newtype wrapper around `Naperian` to derive `Applicative`.
- The definition of `FiniteNaperian`.
- `FiniteNaperian` instances for `Product` and `Compose`.
- A newtype wrapper around `FiniteNaperian` to derive `Foldable`, `Dimension`,
  `Eq1`, `Ord1`, and `Show1`.
  Also included is a default definition of `traverse` to define `Traversable`
  (which cannot be derived with `DerivingVia`).
- The definition of `FiniteHyper` multidimensional version of `FiniteNaperian`,
  which is indexed by a heterogeneous list `HList [a1, ..., an]`.
- Various instances for `HList [a1, ..., an]` including `Bounded` and `Enum`.
- Instances for `FiniteHyper` including `FiniteNaperian`.

The module `Data.Naperian.Tree` implements some type indexed trees and gives
their finite Naperian instances.
Additionally, it uses the category of indexed functors to define a form of
catamorphisms.
Finally, it uses some extra structure of the trees to allow a `mapAccum` style
function to be defined.

The module `Data.Naperian.Examples` shows how to define your own finite Naperian
functor.

### Working with Torch

The module `Torch.Naperian` shows some functions which are similar to Torch
functions, and are implemented in pure Haskell.
The main portion of the is the `Dim` data type and functions for interacting
with it.

The module `Torch.Naperian.Examples` shows how to perform a simple linear
regression using finite Naperian functors and the `ad` library for it's gradient
descent function.
It also uses indexed trees to create a Tree LSTM.
