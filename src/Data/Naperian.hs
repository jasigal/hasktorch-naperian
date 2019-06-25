{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Data.Naperian
Description : Instances for Naperian functors and finite Naperian functors.
Copyright   : (c) Jesse Sigal, 2019

Instances and wrapper classes for Naperian and finite Naperian functors.
-}

module Data.Naperian where

import           Naperian                hiding ( Pair )
import qualified Prelude
import           Prelude                 hiding ( lookup
                                                , length
                                                , replicate
                                                , zipWith
                                                )
import           Data.Functor.Product
import           Data.Functor.Compose
import           Data.Functor.Classes
import           Data.Functor.Identity
import           Data.Foldable
import           Data.Maybe
import           Control.Applicative
import           GHC.TypeLits
import           GHC.Exts

-- | The 'Identity' functor is Naperian with log @()@.
instance Naperian Identity where
  type Log Identity = ()
  positions = Identity ()
  lookup (Identity x) () = x

instance FiniteNaperian Identity where
  type Size Identity = 1

-- | When @f@ and @g@ are Naperian functors, then so is @'Product' f g@ with
-- log @('Log' f) + ('Log' g)@.
instance (Naperian f, Naperian g) => Naperian (Product f g) where
  type Log (Product f g) = Either (Log f) (Log g)
  positions = Pair (fmap Left positions) (fmap Right positions)
  lookup (Pair f g) (Left  x) = lookup f x
  lookup (Pair f g) (Right y) = lookup g y

-- | A 'Product' of finite Naperian functors is finite.
instance (FiniteNaperian f, FiniteNaperian g) => FiniteNaperian (Product f g) where
  type Size (Product f g) = Size f + Size g

-- | When @f@ and @g@ are Naperian functors, then so is @'Compose' f g@ with
-- log @('Log' f) * ('Log' g)@.
instance (Naperian f, Naperian g) => Naperian (Compose f g) where
  type Log (Compose f g) = (Log f, Log g)
  lookup (Compose fg) (x, y) = lookup (lookup fg x) y
  tabulate h = Compose $ tabulate (\x -> tabulate (\y -> h (x, y)))

-- | A 'Product' of finite Naperian functors is finite.
instance (FiniteNaperian f, FiniteNaperian g) => FiniteNaperian (Compose f g) where
  type Size (Compose f g) = Size f GHC.TypeLits.* Size g

-- | A newtype wrapper around a Naperian functor for automatically deriving
-- instances using @DerivingVia@.
newtype WrappedNaperian f a = WrappedNaperian {unWrappedNaperian :: f a}
  deriving Functor

-- | Lifts underlying 'Naperian' instance.
instance Naperian f => Naperian (WrappedNaperian f) where
  type Log (WrappedNaperian f) = Log f
  tabulate = WrappedNaperian . tabulate
  lookup   = lookup . unWrappedNaperian

-- | All Naperian functors are 'Applicative' in a pointwise manner.
instance Naperian f => Applicative (WrappedNaperian f) where
  pure x = tabulate (const x)
  fs <*> xs = tabulate (\i -> lookup fs i (lookup xs i))

-- | The class of Naperian functors which are represented by a finite type.
class (Naperian f, Enum (Log f), Bounded (Log f), KnownNat (Size f)) => FiniteNaperian f where
  -- | The static size of a FiniteNaperian functor as a type-level natural.
  type Size f :: Nat

-- | A newtype wrapper around a finite Naperian functor for automatically
-- deriving instances using @DerivingVia@.
newtype WrappedFiniteNaperian f a = WrappedFiniteNaperian {unWrappedFiniteNaperian :: f a}
  deriving Functor
  deriving (Naperian, Applicative) via (WrappedNaperian f)

instance FiniteNaperian f => FiniteNaperian (WrappedFiniteNaperian f) where
  type Size (WrappedFiniteNaperian f) = Size f

instance KnownNat n => FiniteNaperian (Vector n) where
  type Size (Vector n) = n

-- | All finite Naperian functors can be giving a 'Foldable' instance by using
-- the list instance.
instance FiniteNaperian f => Foldable (WrappedFiniteNaperian f) where
  foldMap f xs = foldMap (f . lookup xs) (enumFromTo minBound maxBound)

-- | All finite Naperian functors can be giving a 'Traversable' instance by
-- using the list instance.
instance FiniteNaperian f => Traversable (WrappedFiniteNaperian f) where
  traverse = traverseFinNap

-- | All finite Naperian functors trivially have a 'Dimension' instance.
instance FiniteNaperian f => Dimension (WrappedFiniteNaperian f) where
  size _ = fromIntegral (natVal' (proxy# :: Proxy# (Size f))) :: Int

-- | All finite Naperian functors have 'Eq1' instances by working pointwise.
instance FiniteNaperian f => Eq1 (WrappedFiniteNaperian f) where
  liftEq f xs ys = and (liftA2 f xs ys)

-- | All finite Naperian functors have an 'Ord1' instance by interpreting them
-- as n-ary homogeneous tuples.
instance FiniteNaperian f => Ord1 (WrappedFiniteNaperian f) where
  liftCompare cmp xs ys = fold (liftA2 cmp xs ys)

-- | A useful instance to allow deriving of 'Show' for 'Product' and 'Compose'.
instance FiniteNaperian f => Show1 (WrappedFiniteNaperian f) where
  liftShowsPrec _ shwL p xs =
    showParen (p > 10)
      $ showChar '<'
      . shwL (Data.Foldable.toList xs)
      . showChar '>'

-- | We cannot use 'DerivingVia' to derive a 'Traversable' instance for finite
-- Naperian functors. Essentially, 'DerivingVia' works using the 'Coercible'
-- type class and its member @coerce@. Tyring to deriving 'Traversable' will
-- attempt to use @coerce@ under the 'Applicative' functor @f@. However, this
-- is only valid if @f@s argument has a nominal role, which is not guarunteed
-- in the signature. For a more detailed explanation, see
-- <https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/ here>.
traverseFinNap
  :: forall a b f t
   . (FiniteNaperian t, Foldable t, Applicative f)
  => (a -> f b)
  -> t a
  -> f (t b)
traverseFinNap f = fmap fromVector . Prelude.traverse f . toVector
 where
  toVector t =
    case
        Naperian.fromList (Data.Foldable.toList t) :: Maybe (Vector (Size t) a)
      of
        Nothing  -> error "t-shaped data should have (Size t) elements."
        Just vec -> vec
  fromVector vec =
    tabulate (\i -> vec `index` (fromJust . finite $ fromEnum i))

-- | A lexicographic enumeration for pairs.
instance (Enum a, Enum b, Bounded a, Bounded b) => Enum (a, b) where
  toEnum i = (toEnum @a (i `div` bound), toEnum @b (i `mod` bound))
    where bound = fromEnum (maxBound @a) + 1
  fromEnum (x, y) = fromEnum y + ((fromEnum (maxBound @a) + 1) * fromEnum x)

-- | A bound for sums where the left is ordered below the right.
instance (Bounded a, Bounded b) => Bounded (Either a b) where
  minBound = Left minBound
  maxBound = Right maxBound

-- | A left-biased enumeration of a sum.
instance (Enum a, Enum b, Bounded a, Bounded b) => Enum (Either a b) where
  toEnum i | i > bound = Right (toEnum (i - bound - 1))
           | otherwise = Left (toEnum i)
    where bound = fromEnum (maxBound @a)
  fromEnum (Left  x) = fromEnum x
  fromEnum (Right y) = fromEnum (maxBound @a) + 1 + fromEnum y

instance KnownNat n => Bounded (Finite n) where
  minBound = Fin 0
  maxBound = Fin ((fromIntegral (natVal' (proxy# :: Proxy# n)) :: Int) - 1)

instance KnownNat n => Enum (Finite n) where
  toEnum i = fromJust (finite i)
  fromEnum (Fin i) = i

deriving via (WrappedFiniteNaperian (Vector n)) instance
  (KnownNat n) => Show1 (Vector n)

deriving via (WrappedFiniteNaperian (Vector n)) instance
  (KnownNat n) => Eq1 (Vector n)
