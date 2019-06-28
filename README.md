# hasktorch-naperian

## Goal

To provide a finite Naperian functor based interface to `hasktorch`.

## Status

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

The module `Data.Naperian.Examples` shows how to define your own finite Naperian
functor.

### Model of working with Torch

The module `Torch.Naperian` shows some functions which are similar to Torch
functions, and are implemented in pure Haskell.

The module `Torch.Naperian.Examples` shows how to perform a simple linear
regression using finite Naperian functors and the `ad` library for it's gradient
descent function.

### Integrating finite Naperian functors and Torch

#### Implemented code

The module `Data.Naperian.Hybrid` is an attempt to sketch out a method to
integrate Torch tensors with finite Naperian functors.

We define a mock foreign tensor type `ForeignTensor (ns :: [Nat]) a` to model a
Torch tensor wrapped a static list of dimensions.
We also define `NestedVectors (ns :: [Nat]) a` for ease of use.
We provide the type class `Transform` to automatically convert between
`FiniteHyper` and `NestedVectors`.

The main content of the module is the `Dim` data type.
A `Dim` is either a native Haskell value of a `FiniteHyper`, or a foreign tensor
of value `ForeignTensor` paired with deferred functions.
The accumulation of deferred functions allows for a "morally correct" functor
instance of `Dim`.

We provide example functions for forcing deferred functions and packing (i.e.
sending to Torch) as well as how to dispatch on types.
Finally, we show how function such as negate and transpose could be implemented.
Note that for negate, it could be possible to avoid the use of the Haskell
function `negate` by calling the corresponding Torch function on zero
dimensional tensors.
This would allow all pointwise functions to be created with Template Haskell.

#### Evaluation of approach

Given the precondition of using the existing Haskell type class hierarchy, the
approach seems like the only available.

The `Functor` type class and others require actual polymorphism, which means
that if the user wishes to apply a function to `Dim` which produces a type
unsupported by the Torch library, we must produce a Haskell type.
Our method is relatively optimal however.
By having only a "morally correct" functor instance (by tracking if `fmap` has
ever been called), we can dispatch supported functions to Torch.
Additionally, natural transformations like transpose commute with `fmap` by
naturality and so can be applied to the foreign tensor under deferred functions.

One big drawback is that to support a function from the Torch library, we would
have to write our own version in the case of a Haskell type in `Dim`.
For pointwise functions, we could possibly defer to Torch via FFI, but for
functions like transpose and reductions, we would have to reimplement them.
This is a very high cost.

## Building with `ffi-experimental`

Have `ffi-experimental` in the same parent directory as `hasktorch-naperian` and
then run:

```bash
stack build --extra-lib-dirs="../ffi-experimental/deps/libtorch/lib/" --extra-lib-dirs="../ffi-experimental/deps/mklml/lib/" --extra-include-dirs="../ffi-experimental/deps/libtorch/include" --extra-include-dirs="../ffi-experimental/deps/libtorch/include/torch/csrc/api/include"
```
