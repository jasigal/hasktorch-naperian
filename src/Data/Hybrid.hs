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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Hybrid where

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

pack :: forall a fs ns. (DType a, Transform ns fs, All FiniteNaperian fs) => FiniteHyper fs a -> ForeignTensor ns a
pack fa = pack' $ concretize fa
  where
    pack' :: DType a => NestedVectors ns a -> ForeignTensor ns a
    pack' (Single x) = ZeroDim x
    pack' (Nested (Single xs)) = OneDim xs
    pack' (Nested (Nested (Single xss))) = TwoDim xss

-- Use to keep track of deferred calls to fmap with the ability to check if
-- fmap has ever been called.
data Deferred a b where
  Id :: Deferred a a
  Deferred :: (a -> b) -> Deferred a b

ap :: Deferred a b -> a -> b
ap Id x = x
ap (Deferred f) x = f x

-- Our hybrid data type which can be a foreign or native tensor. It achieves
-- moral functorality by deferring fmap calls on a foreign tensor.
data Dim (fs :: [Type -> Type]) a where
  Foreign :: (All FiniteNaperian fs, Transform ns fs, ns ~ MapSize fs)
          => ForeignTensor ns b -> Deferred b a -> Dim fs a
  Native :: All FiniteNaperian fs => FiniteHyper fs a -> Dim fs a

instance Show a => Show (Dim fs a) where
  show (Foreign t f) = show ""
  show (Native n) = case n of
    Scalar x -> show x
    Prism x -> ""

-- Functor instance for the Hybrid type. Note this is not literally
-- law-abiding as:
-- > fmap id x != x, so fmap id != id
-- However, if we hide the deferral function from the user (and its effects)
-- then this is morally acceptable. Essentially, it is a functor under
-- dimMaterialize, meaning:
-- > dimMaterialize (fmap id x) == dimMaterialize x
instance Functor (Dim ns) where
  fmap f (Foreign t Id) = Foreign t (Deferred f)
  fmap f (Foreign t (Deferred g)) = Foreign t (Deferred (f . g))
  fmap f (Native l) = Native $ fmap f l

-- Helper function which would not be exported.
isForeign :: Dim ns a -> Bool
isForeign (Foreign _ _) = True
isForeign _ = False

-- Turn a hybrid into a nested vectors, i.e. the corresponding actual functor.
dimMaterialize :: Dim fs a -> FiniteHyper fs a
dimMaterialize (Foreign t f) = fmap (ap f) $ materialize t
dimMaterialize (Native n) = n

x :: Dim '[Vector 2, Vector 2] Int
x = Foreign (TwoDim [[1 :: Int, 2], [3, 4]]) Id

y :: Dim '[Vector 2, Vector 2] Int
y = Native $ Prism $ Prism $ Scalar [[1, 2], [3, 4]]

-- Force any deferred calls, will marshal foreign to native if needed.
forceDeferred :: Dim fs a -> Dim fs a
forceDeferred t@(Foreign _ Id) = t
forceDeferred (Foreign t f) = Native (fmap (ap f) $ materialize t)
forceDeferred n@(Native _) = n

-- Packs a native into a foreign
packForeign :: forall a fs. DType a => Dim fs a -> Dim fs a
packForeign t@(Foreign _ Id) = t
packForeign (Foreign t f) = Foreign (pack @a @fs @(MapSize fs) . fmap (ap f) $ materialize t) Id
packForeign (Native n) = Foreign (pack @a @fs @(MapSize fs) n) Id

-- If we have a Typeable, we can do type level equality and dispatch based on
-- if a DType instance exists.
packOrForce :: forall a fs. Typeable a => Dim fs a -> Dim fs a
packOrForce d = case checkDType @a of
  Just Dict -> packForeign d
  Nothing -> forceDeferred d

-- Modelling C++ calling a Haskell function. If a Haskell function takes and
-- produces DTypes, we can call it from C++ (modelled here with tensorApply)
-- to avoid marshalling.
foreignCall :: forall a b fs. (Typeable a, Typeable b) => (a -> b) -> Dim fs a -> Dim fs b
foreignCall f d = case (checkDType @a, checkDType @b) of
  (Just Dict, Just Dict) -> case packForeign d of
    (Foreign t Id) -> Foreign (tensorApply f t) Id
    _              -> error "packForeign failed"
  (_, _) -> fmap f d

-- As transpose is a natural transformation, it commutes with fmap and so we
-- can apply it under all deferred fmaps.
dimTranspose :: forall f g a. Dim '[f, g] a -> Dim '[g, f] a
dimTranspose (Foreign t f) = Foreign (dynamicTranspose t) f
dimTranspose (Native (Prism (Prism (Scalar vss)))) = Native
  (Prism (Prism (Scalar (transpose vss))))

-- For each Torch function, we can define a function like this which uses
-- Torch's implementation if there are no deferred calls. We also could avoid
-- writing a native instance by packing and then using the dynamic version.
dimNegate :: forall a fs. (Num a, DType a) => Dim fs a -> Dim fs a
dimNegate (Foreign t Id) = case dtype @a of
  DTypeDouble -> Foreign (dynamicNegate @Double t) Id
  DTypeInt -> Foreign (dynamicNegate @Int t) Id
dimNegate (Foreign t (Deferred f)) = Foreign t (Deferred (negate . f))
dimNegate (Native l) = Native (fmap negate l)

-- Packs first to force the use of Torch's implementation.
dimNegate' :: forall a fs. (Num a, DType a) => Dim fs a -> Dim fs a
dimNegate' = dimNegate . packForeign