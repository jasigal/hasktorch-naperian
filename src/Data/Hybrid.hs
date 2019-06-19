{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Hybrid where

import           GHC.TypeLits
import           Data.Type.Bool
import           Data.Proxy (Proxy(..))
import           Data.Kind (Type, Constraint)
import           Data.Type.Equality
import           Naperian

type family IsDType t where
  IsDType Double = True
  IsDType Int = True
  IsDType _ = False

data DTypeRep a where
  DTypeDouble :: DTypeRep Double
  DTypeInt :: DTypeRep Int

class (IsDType a ~ True) => DType a where
  dtype :: DType a => DTypeRep a

instance DType Double where
  dtype = DTypeDouble

instance DType Int where
  dtype = DTypeInt

data ForeignTensor (ns :: [Nat]) a where
  ZeroDim :: DType a => a -> ForeignTensor '[] a
  OneDim :: (DType a, KnownNat n) => Vector n a -> ForeignTensor '[n] a
  TwoDim :: (DType a, KnownNat n, KnownNat m)
    => Vector m (Vector n a)
    -> ForeignTensor '[n, m] a

deriving instance Show a => Show (ForeignTensor ns a)

tensorApply
  :: DType b => (a -> b) -> (ForeignTensor ns a -> ForeignTensor ns b)
tensorApply f (ZeroDim x) = ZeroDim $ f x
tensorApply f (OneDim x) = OneDim $ fmap f x
tensorApply f (TwoDim x) = TwoDim $ fmap (fmap f) x

dynamicTranspose :: ForeignTensor '[n, m] a -> ForeignTensor '[m, n] a
dynamicTranspose (TwoDim vss) = TwoDim (transpose vss)

data NestedVectors (ns :: [Nat]) a where
  Single :: a -> NestedVectors '[] a
  Nested
    :: KnownNat n => NestedVectors ns (Vector n a) -> NestedVectors (n:ns) a

deriving instance Show a => Show (NestedVectors ns a)

instance Functor (NestedVectors ns) where
  fmap f (Single a) = Single (f a)
  fmap f (Nested n) = Nested $ fmap (fmap f) n

materialize :: ForeignTensor ns b -> NestedVectors ns b
materialize (ZeroDim x) = Single x
materialize (OneDim x) = Nested $ Single x
materialize (TwoDim x) = Nested $ Nested $ Single x

data Dim (n :: [Nat]) a where
  Foreign :: ForeignTensor ns b -> (b -> a) -> Dim ns a
  Native :: NestedVectors ns a -> Dim ns a

instance Show a => Show (Dim ns a) where
  show (Foreign t f) = show (fmap f $ materialize t)
  show (Native n) = show n

instance Functor (Dim ns) where
  fmap f (Foreign t g) = Foreign t (f . g)
  fmap f (Native l) = Native $ fmap f l

x :: Dim '[2, 2] Int
x = Foreign (TwoDim [[1 :: Int, 2], [3, 4]]) id

y :: Dim '[2, 2] Int
y = Native $ Nested $ Nested $ Single [[1, 2], [3, 4]]

dimTranspose :: forall n m a. Dim '[n, m] a -> Dim '[m, n] a
dimTranspose (Foreign t f) = Foreign (dynamicTranspose t) f
dimTranspose (Native (Nested (Nested (Single vss)))) = Native
  (Nested (Nested (Single (transpose vss))))