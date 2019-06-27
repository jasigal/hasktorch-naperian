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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Data.Naperian
Description : Instances for Naperian functors and finite Naperian functors.
Copyright   : (c) Jesse Sigal, 2019

Instances and wrapper classes for Naperian and finite Naperian functors.
-}

module Data.Naperian where

import           Naperian                hiding ( Pair
                                                , Hyper(..)
                                                )
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
import           Data.Kind                      ( Type )

--------------------------------------------------------------------------------
-- Finite Naperian functors.
--------------------------------------------------------------------------------

-- | The class of Naperian functors which are represented by a finite type.
class (Naperian f, Enum (Log f), Bounded (Log f), KnownNat (Size f)) => FiniteNaperian f where
  -- | The static size of a FiniteNaperian functor as a type-level natural.
  type Size f :: Nat

--------------------------------------------------------------------------------
-- Instances for basic buidling blocks.
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Newtype wrappers for DerivingVia with Naperians.
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Newtype wrappers for DerivingVia with FiniteNaperian.
--------------------------------------------------------------------------------

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

fromVector :: (n ~ Size f, FiniteNaperian f) => Vector n a -> f a
fromVector vec = tabulate (\i -> vec `index` (fromJust . finite $ fromEnum i))

toVector :: (n ~ Size f, FiniteNaperian f) => f a -> Vector n a
toVector fa =
  case
      Naperian.fromList (Data.Foldable.toList $ WrappedFiniteNaperian fa)
    of
      Nothing  -> error "t-shaped data should have (Size t) elements."
      Just vec -> vec
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

--------------------------------------------------------------------------------
-- Auxiliary Enum and Bounded instances needed for FiniteNaperian instances of
-- basic building blocks.
--------------------------------------------------------------------------------

-- | A lexicographic enumeration for pairs.
instance (Enum a, Enum b, Bounded a, Bounded b) => Enum (a, b) where
  toEnum i | fromEnum (maxBound @a) == 0 = (minBound, toEnum i)
           | -- single element type
             fromEnum (maxBound @b) == 0 = (toEnum i, minBound)
           | -- single element type
             otherwise = (toEnum @a (i `div` bound), toEnum @b (i `mod` bound))
    where bound = fromEnum (maxBound @a) + 1
  fromEnum (x, y)
    | fromEnum (maxBound @a) == 0 = fromEnum y
    | -- single element type
      fromEnum (maxBound @b) == 0 = fromEnum x
    | -- single element type
      otherwise = fromEnum y + ((fromEnum (maxBound @a) + 1) * fromEnum x)

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

--------------------------------------------------------------------------------
-- Vector related instances.
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- FiniteNaperian version of Hyper.
--------------------------------------------------------------------------------

type family All (pred :: a -> Constraint) (l :: [a]) :: Constraint where
  All _    '[]      = ()
  All pred (h ': t) = (pred h, All pred t)

type family MapLog (l :: [a]) :: [b] where
  MapLog '[]      = '[]
  MapLog (x : xs) = Log x : MapLog xs

type family Prod (ns :: [Nat]) :: Nat where
  Prod '[]      = 0
  Prod (n : ns) = n GHC.TypeLits.* Prod ns

data FiniteHyper :: [Type -> Type] -> Type -> Type where
  Scalar :: a -> FiniteHyper '[] a
  Prism :: (FiniteNaperian f, All FiniteNaperian fs) => FiniteHyper fs (f a) -> FiniteHyper (f : fs) a

point' :: FiniteHyper '[] a -> a
point' (Scalar a) = a

crystal' :: FiniteHyper (f : fs) a -> FiniteHyper fs (f a)
crystal' (Prism x) = x

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a : as)

instance Eq (HList '[]) where
  _ == _ = True
instance (Eq a, Eq (HList as)) => Eq (HList (a : as)) where
  (HCons x xs) == (HCons y ys) = (x == y) && (xs == ys)

instance Show (HList '[]) where
  show _ = "HNil"
instance (Show a, Show (HList as)) => Show (HList (a : as)) where
  show (HCons a as) = show a ++ " <: " ++ show as

instance Bounded (HList '[]) where
  minBound = HNil
  maxBound = HNil
instance (Bounded a, Bounded (HList as)) => Bounded (HList (a : as)) where
  minBound = HCons minBound minBound
  maxBound = HCons maxBound maxBound

instance Enum (HList '[]) where
  toEnum 0 = HNil
  fromEnum HNil = 0
instance (Bounded a, Bounded (HList as), Enum a, Enum (HList as)) => Enum (HList (a : as)) where
  toEnum i
    | fromEnum (maxBound @a) == 0 = HCons minBound (toEnum i)
    | -- single element type
      fromEnum (maxBound @(HList as)) == 0 = HCons (toEnum i) minBound
    |   -- single element type
      otherwise = HCons (toEnum @a (i `div` bound))
                        (toEnum @(HList as) (i `mod` bound))
    where bound = fromEnum (maxBound @a) + 1
  fromEnum (HCons x y)
    | fromEnum (maxBound @a) == 0 = fromEnum y
    | -- single element type
      fromEnum (maxBound @(HList as)) == 0 = fromEnum x
    | -- single element type
      otherwise = fromEnum y + ((fromEnum (maxBound @a) + 1) * fromEnum x)

--------------------------------------------------------------------------------
-- Instances for FiniteHyper
--------------------------------------------------------------------------------

instance Show1 (FiniteHyper '[]) where
  liftShowsPrec sp _ d (Scalar a) = sp d a
instance (Show1 f, Show1 (FiniteHyper fs)) => Show1 (FiniteHyper (f : fs)) where
  liftShowsPrec sp sl d (Prism fa) = liftShowsPrec sp' sl' d fa
   where
    sp' = liftShowsPrec sp sl
    sl' = liftShowList sp sl

instance Show a => Show (FiniteHyper '[] a) where
  showsPrec = showsPrec1
instance (Show1 f, Show1 (FiniteHyper fs), Show a) => Show (FiniteHyper (f : fs) a) where
  showsPrec = showsPrec1

instance Functor (FiniteHyper fs) where
  fmap f (Scalar x) = Scalar (f x)
  fmap f (Prism  x) = Prism ((fmap . fmap) f x)

instance Naperian (FiniteHyper '[]) where
  type Log (FiniteHyper '[]) = HList '[]
  positions = Scalar HNil
  lookup (Scalar x) _ = x
instance ( FiniteNaperian f
         , All FiniteNaperian fs
         , FiniteNaperian (FiniteHyper fs)
         , Log (FiniteHyper fs) ~ HList (MapLog fs)
         ) => Naperian (FiniteHyper (f : fs)) where
  type Log (FiniteHyper (f : fs)) = HList (Log f : MapLog fs)
  lookup (Prism xs) (HCons i is) = lookup (lookup xs is) i
  tabulate h = Prism $ tabulate (\x -> tabulate (\y -> h (HCons y x)))

instance FiniteNaperian (FiniteHyper '[]) where
  type Size (FiniteHyper '[]) = 1
instance ( FiniteNaperian f
         , All FiniteNaperian fs
         , FiniteNaperian (FiniteHyper fs)
         , Log (FiniteHyper fs) ~ HList (MapLog fs)
         ) => FiniteNaperian (FiniteHyper (f : fs)) where
  type Size (FiniteHyper (f : fs)) = Size f GHC.TypeLits.* Size (FiniteHyper fs)

deriving via (WrappedNaperian (FiniteHyper '[])) instance Applicative (FiniteHyper '[])
deriving via (WrappedNaperian (FiniteHyper (f : fs))) instance
  ( FiniteNaperian f
  , All FiniteNaperian fs
  , FiniteNaperian (FiniteHyper fs)
  , Log (FiniteHyper fs) ~ HList (MapLog fs)
  ) => Applicative (FiniteHyper (f : fs))

deriving via (WrappedFiniteNaperian (FiniteHyper '[])) instance Foldable (FiniteHyper '[])
deriving via (WrappedFiniteNaperian (FiniteHyper (f : fs))) instance
  ( FiniteNaperian f
  , All FiniteNaperian fs
  , FiniteNaperian (FiniteHyper fs)
  , Log (FiniteHyper fs) ~ HList (MapLog fs)
  ) => Foldable (FiniteHyper (f : fs))

instance Traversable (FiniteHyper '[]) where
  traverse = traverseFinNap
instance
  ( FiniteNaperian f
  , All FiniteNaperian fs
  , FiniteNaperian (FiniteHyper fs)
  , Log (FiniteHyper fs) ~ HList (MapLog fs)
  ) => Traversable (FiniteHyper (f : fs)) where
  traverse = traverseFinNap

deriving via (WrappedFiniteNaperian (FiniteHyper '[])) instance Dimension (FiniteHyper '[])
deriving via (WrappedFiniteNaperian (FiniteHyper (f : fs))) instance
  ( FiniteNaperian f
  , All FiniteNaperian fs
  , FiniteNaperian (FiniteHyper fs)
  , Log (FiniteHyper fs) ~ HList (MapLog fs)
  ) => Dimension (FiniteHyper (f : fs))

deriving via (WrappedFiniteNaperian (FiniteHyper '[])) instance Eq1 (FiniteHyper '[])
deriving via (WrappedFiniteNaperian (FiniteHyper (f : fs))) instance
  ( FiniteNaperian f
  , All FiniteNaperian fs
  , FiniteNaperian (FiniteHyper fs)
  , Log (FiniteHyper fs) ~ HList (MapLog fs)
  ) => Eq1 (FiniteHyper (f : fs))

deriving via (WrappedFiniteNaperian (FiniteHyper '[])) instance Ord1 (FiniteHyper '[])
deriving via (WrappedFiniteNaperian (FiniteHyper (f : fs))) instance
  ( FiniteNaperian f
  , All FiniteNaperian fs
  , FiniteNaperian (FiniteHyper fs)
  , Log (FiniteHyper fs) ~ HList (MapLog fs)
  ) => Ord1 (FiniteHyper (f : fs))
