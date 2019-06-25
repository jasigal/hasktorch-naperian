{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module Data.Naperian.Examples where

import           Data.Typeable
import           Data.Data
import           Data.Naperian
import           Naperian
import           Data.Functor.Classes
import           Data.Functor.Compose

data Triple a = Triple a a a
  deriving (Show, Eq, Functor, Typeable, Data)
  deriving Applicative via (WrappedNaperian Triple)
  deriving (Foldable, Dimension, Eq1, Ord1, Show1) via (WrappedFiniteNaperian Triple)
instance Traversable Triple where
  traverse = traverseFinNap
instance FiniteNaperian Triple where
  type Size Triple = 3

data Three = Zero | One | Two
  deriving (Show, Eq, Enum, Bounded)

newtype Nonuple a = Nonuple {unNonuple :: Compose Triple Triple a}
  deriving (Show, Eq, Functor, Naperian, FiniteNaperian)
  deriving Applicative via (WrappedNaperian Nonuple)
  deriving (Foldable, Dimension, Eq1, Ord1) via (WrappedFiniteNaperian Nonuple)
instance Traversable Nonuple where
  traverse = traverseFinNap

n :: Nonuple Int
n = Nonuple (Compose (Triple x0 x1 x2))
 where
  x0 = Triple 0 1 2
  x1 = Triple 3 4 5
  x2 = Triple 6 7 8

instance Naperian Triple where
  type Log Triple = Three
  positions = Triple Zero One Two
  lookup (Triple x _ _) Zero = x
  lookup (Triple _ y _) One  = y
  lookup (Triple _ _ z) Two  = z
