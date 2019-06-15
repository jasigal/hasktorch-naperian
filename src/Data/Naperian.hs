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

instance Naperian Identity where
  type Log Identity = ()
  positions = Identity ()
  lookup (Identity x) () = x

instance (Naperian f, Naperian g) => Naperian (Product f g) where
  type Log (Product f g) = Either (Log f) (Log g)
  positions = Pair (fmap Left positions) (fmap Right positions)
  lookup (Pair f g) (Left  x) = lookup f x
  lookup (Pair f g) (Right y) = lookup g y

deriving via (WrappedFiniteNaperian (Product f g)) instance
  (FiniteNaperian f, FiniteNaperian g, Dimension f, Dimension g) => Dimension (Product f g)

instance (Naperian f, Naperian g) => Naperian (Compose f g) where
  type Log (Compose f g) = (Log f, Log g)
  lookup (Compose fg) (x, y) = lookup (lookup fg x) y
  tabulate h = Compose $ tabulate (\x -> tabulate (\y -> h (x, y)))

deriving via (WrappedFiniteNaperian (Compose f g)) instance
  (FiniteNaperian f, FiniteNaperian g, Dimension f, Dimension g) => Dimension (Compose f g)

newtype WrappedNaperian f a = WrappedNaperian {unWrappedNaperian :: f a}
  deriving Functor

instance Naperian f => Naperian (WrappedNaperian f) where
  type Log (WrappedNaperian f) = Log f
  tabulate = WrappedNaperian . tabulate
  lookup   = lookup . unWrappedNaperian

instance Naperian f => Applicative (WrappedNaperian f) where
  pure x = tabulate (const x)
  fs <*> xs = tabulate (\i -> lookup fs i (lookup xs i))

type FiniteNaperian f = (Naperian f, Enum (Log f), Bounded (Log f))

newtype WrappedFiniteNaperian f a = WrappedFiniteNaperian {unWrappedFiniteNaperian :: f a}
  deriving Functor
  deriving (Naperian, Applicative) via (WrappedNaperian f)

instance FiniteNaperian f => Foldable (WrappedFiniteNaperian f) where
  foldMap f xs = foldMap (f . lookup xs) (enumFromTo minBound maxBound)

instance FiniteNaperian f => Traversable (WrappedFiniteNaperian f) where
  traverse = traverseFinNap

instance FiniteNaperian f => Dimension (WrappedFiniteNaperian f) where
  size _ = fromEnum (maxBound @(Log f)) + 1

instance FiniteNaperian f => Eq1 (WrappedFiniteNaperian f) where
  liftEq f xs ys = and (liftA2 f xs ys)

instance FiniteNaperian f => Ord1 (WrappedFiniteNaperian f) where
  liftCompare cmp xs ys = fold (liftA2 cmp xs ys)

instance FiniteNaperian f => Show1 (WrappedFiniteNaperian f) where
  liftShowsPrec _ shwL p xs =
    showParen (p > 10)
      $ showChar '<'
      . shwL (Data.Foldable.toList xs)
      . showChar '>'

traverseFinNap
  :: (FiniteNaperian t, Foldable t, Applicative f)
  => (a -> f b)
  -> t a
  -> f (t b)
traverseFinNap f = fmap fromList . Prelude.traverse f . Data.Foldable.toList
  where fromList xs = tabulate (\i -> xs !! fromEnum i)

instance (Enum a, Enum b, Bounded a, Bounded b) => Enum (a, b) where
  toEnum i = (toEnum @a (i `div` bound), toEnum @b (i `mod` bound))
    where bound = fromEnum (maxBound @a) + 1
  fromEnum (x, y) = fromEnum y + ((fromEnum (maxBound @a) + 1) * fromEnum x)

instance (Bounded a, Bounded b) => Bounded (Either a b) where
  minBound = Left minBound
  maxBound = Right maxBound

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
