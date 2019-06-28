{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Inexed where

import           GHC.TypeLits
import           Data.Type.Bool
import           Data.Proxy (Proxy(..))
import           Data.Kind (Type, Constraint)
import           Data.Type.Equality (testEquality)
import           Type.Reflection
import           Data.Constraint
import           Naperian hiding (Hyper(..))
import           Data.Naperian
import           Data.Functor.Classes

-- DType singleton
data DTypeRep a where
  DTypeDouble :: DTypeRep Double
  DTypeInt :: DTypeRep Int

class Typeable a => DType a where
  dtype :: DTypeRep a

instance DType Double where
  dtype = DTypeDouble

instance DType Int where
  dtype = DTypeInt

checkDType :: forall a. Typeable a => Maybe (Dict (DType a))
checkDType
  | Just Refl <- testEquality (typeRep @a) (typeRep @Double) = Just Dict
  | Just Refl <- testEquality (typeRep @a) (typeRep @Int)    = Just Dict
  | otherwise = Nothing

-- Stand-in for a Torch/ATen tensor.
data ForeignTensor (ns :: [Nat]) a where
  ZeroDim :: DType a => a -> ForeignTensor '[] a
  OneDim :: (DType a, KnownNat n) => Vector n a -> ForeignTensor '[n] a
  TwoDim :: (DType a, KnownNat n, KnownNat m)
    => Vector m (Vector n a)
    -> ForeignTensor '[n, m] a

deriving instance Show a => Show (ForeignTensor ns a)

tensorApply
  :: (DType a, DType b) => (a -> b) -> (ForeignTensor ns a -> ForeignTensor ns b)
tensorApply f (ZeroDim x) = ZeroDim $ f x
tensorApply f (OneDim x) = OneDim $ fmap f x
tensorApply f (TwoDim x) = TwoDim $ fmap (fmap f) x

dynamicTranspose :: ForeignTensor '[n, m] a -> ForeignTensor '[m, n] a
dynamicTranspose (TwoDim vss) = TwoDim (transpose vss)

dynamicNegate :: (DType a, Num a) => ForeignTensor ns a -> ForeignTensor ns a
dynamicNegate = tensorApply negate

-- Nested Haskell vectors, stand-in for nested Dimensions in a Hyper-like
-- construction.
data NestedVectors (ns :: [Nat]) a where
  Single :: a -> NestedVectors '[] a
  Nested
    :: KnownNat n => NestedVectors ns (Vector n a) -> NestedVectors (n:ns) a

deriving instance Show a => Show (NestedVectors ns a)

instance Functor (NestedVectors ns) where
  fmap f (Single a) = Single (f a)
  fmap f (Nested n) = Nested $ fmap (fmap f) n

type family MapSize (fs :: [Type -> Type]) :: [Nat] where
  MapSize '[]      = '[]
  MapSize (f : fs) = Size f : MapSize fs

class Transform (ns :: [Nat]) (fs :: [Type -> Type]) | fs -> ns where
  generalize :: All FiniteNaperian fs => NestedVectors ns a -> FiniteHyper fs a
  concretize :: All FiniteNaperian fs => FiniteHyper fs a -> NestedVectors ns a

instance Transform '[] '[] where
  generalize (Single x) = Scalar x
  concretize (Scalar x) = Single x

instance (n ~ Size f, FiniteNaperian f, Transform ns fs) => Transform (n : ns) (f : fs) where
  generalize (Nested v) = Prism (generalize (fmap fromVector v))
  concretize (Prism fa) = Nested (concretize (fmap toVector fa))

-- Fake marshalling functions
materialize :: (Transform ns fs, All FiniteNaperian fs) => ForeignTensor ns a -> FiniteHyper fs a
materialize (ZeroDim x) = generalize (Single x)
materialize (OneDim x) = generalize (Nested $ Single x)
materialize (TwoDim x) = generalize (Nested . Nested $ Single x)

materialize' :: ForeignTensor ns a -> NestedVectors ns a
materialize' (ZeroDim x) = Single x
materialize' (OneDim x) = Nested $ Single x
materialize' (TwoDim x) = Nested . Nested $ Single x

pack :: forall a fs ns. (DType a, Transform ns fs, All FiniteNaperian fs) => FiniteHyper fs a -> ForeignTensor ns a
pack fa = pack' $ concretize fa
  where
    pack' :: DType a => NestedVectors ns a -> ForeignTensor ns a
    pack' (Single x) = ZeroDim x
    pack' (Nested (Single xs)) = OneDim xs
    pack' (Nested (Nested (Single xss))) = TwoDim xss

data Location = F | N

-- Our hybrid data type which can be a foreign or native tensor.
data Dim (loc :: Location) (fs :: [Type -> Type]) a where
  Foreign :: (DType a, All FiniteNaperian fs, Transform (MapSize fs) fs)
          => ForeignTensor (MapSize fs) a -> Dim F fs a
  Native :: (FiniteNaperian (FiniteHyper fs), All FiniteNaperian fs, Transform (MapSize fs) fs)
         => FiniteHyper fs a -> Dim N fs a

instance Show a => Show (Dim loc fs a) where
  show (Foreign t) = show (materialize' t)
  show (Native n) = show (concretize n)

instance Functor (Dim N fs) where
  fmap f (Native n) = Native $ fmap f n

instance (FiniteNaperian (FiniteHyper fs), All FiniteNaperian fs, Transform (MapSize fs) fs) => Naperian (Dim N fs) where
    type Log (Dim N fs) = Log (FiniteHyper fs)
    lookup (Native n) = Naperian.lookup n
    tabulate f = Native $ tabulate f

-- Turn a hybrid into a `FiniteHyper`, i.e. the corresponding actual functor.
dimMaterialize :: Dim loc fs a -> FiniteHyper fs a
dimMaterialize (Foreign t) = materialize t
dimMaterialize (Native n) = n

x :: Dim F '[Vector 2, Vector 2] Int
x = Foreign (TwoDim [[1 :: Int, 2], [3, 4]])

y :: Dim N '[Vector 2, Vector 2] Int
y = Native $ Prism $ Prism $ Scalar [[1, 2], [3, 4]]

-- Packs a native into a foreign
packForeign :: DType a => Dim N fs a -> Dim F fs a
packForeign (Native n) = Foreign (pack n)

unpackForeign :: FiniteNaperian (FiniteHyper fs) => Dim F fs a -> Dim N fs a
unpackForeign (Foreign t) = Native (materialize t)

-- As transpose is a natural transformation, it commutes with fmap and so we
-- can apply it under all deferred fmaps.
dimTranspose :: Dim loc '[f, g] a -> Dim loc '[g, f] a
dimTranspose (Foreign t) = Foreign (dynamicTranspose t)
dimTranspose (Native (Prism (Prism (Scalar vss)))) = Native
  (Prism (Prism (Scalar (transpose vss))))

-- For each Torch function, we can define a function like this which uses
-- Torch's implementation.
dimNegate :: forall a fs. (Num a, DType a) => Dim F fs a -> Dim F fs a
dimNegate (Foreign t) = case dtype @a of
  DTypeDouble -> Foreign (dynamicNegate @Double t)
  DTypeInt -> Foreign (dynamicNegate @Int t)