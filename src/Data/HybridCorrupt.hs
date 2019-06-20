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

module Data.HybridCorrupt where

import           GHC.TypeLits
import           Data.Type.Bool
import           Data.Proxy (Proxy(..))
import           Data.Kind (Type, Constraint)
import           Data.Type.Equality
import           Type.Reflection
import           Naperian

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

data Dict ctxt where
  Dict :: ctxt => Dict ctxt

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

-- Fake marshalling functions
materialize :: ForeignTensor ns a -> NestedVectors ns a
materialize (ZeroDim x) = Single x
materialize (OneDim x) = Nested $ Single x
materialize (TwoDim x) = Nested $ Nested $ Single x

pack :: DType a => NestedVectors ns a -> ForeignTensor ns a
pack (Single x) = ZeroDim x
pack (Nested (Single xs)) = OneDim xs
pack (Nested (Nested (Single xss))) = TwoDim xss

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
data Dim (n :: [Nat]) a where
  Foreign :: ForeignTensor ns b -> Deferred b a -> Dim ns a
  Native :: NestedVectors ns a -> Dim ns a

instance Show a => Show (Dim ns a) where
  show (Foreign t f) = show (fmap (ap f) $ materialize t)
  show (Native n) = show n

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
dimMaterialize :: Dim ns a -> NestedVectors ns a
dimMaterialize (Foreign t f) = fmap (ap f) $ materialize t
dimMaterialize (Native n) = n

x :: Dim '[2, 2] Int
x = Foreign (TwoDim [[1 :: Int, 2], [3, 4]]) Id

y :: Dim '[2, 2] Int
y = Native $ Nested $ Nested $ Single [[1, 2], [3, 4]]

-- Force any deferred calls, will marshal foreign to native if needed.
forceDeferred :: Dim ns a -> Dim ns a
forceDeferred t@(Foreign _ Id) = t
forceDeferred (Foreign t f) = Native (fmap (ap f) $ materialize t)
forceDeferred n@(Native _) = n

-- Packs a native into a foreign
packForeign :: DType a => Dim ns a -> Dim ns a
packForeign t@(Foreign _ Id) = t
packForeign (Foreign t f) = Foreign (pack . fmap (ap f) $ materialize t) Id
packForeign (Native n) = Foreign (pack n) Id

-- If we have a Typeable, we can do type level equality and dispatch based on
-- if a DType instance exists.
packOrForce :: forall a ns. Typeable a => Dim ns a -> Dim ns a
packOrForce d = case checkDType @a of
  Just Dict -> packForeign d
  Nothing -> forceDeferred d

-- Modelling C++ calling a Haskell function. If a Haskell function takes and
-- produces DTypes, we can call it from C++ (modelled here with tensorApply)
-- to avoid marshalling.
foreignCall :: forall a b ns. (Typeable a, Typeable b) => (a -> b) -> Dim ns a -> Dim ns b
foreignCall f d = case (checkDType @a, checkDType @b) of
  (Just Dict, Just Dict) -> case packForeign d of
    (Foreign t Id) -> Foreign (tensorApply f t) Id
    _              -> error "packForeign failed"
  (_, _) -> fmap f d

-- As transpose is a natural transformation, it commutes with fmap and so we
-- can apply it under all deferred fmaps.
dimTranspose :: forall n m a. Dim '[n, m] a -> Dim '[m, n] a
dimTranspose (Foreign t f) = Foreign (dynamicTranspose t) f
dimTranspose (Native (Nested (Nested (Single vss)))) = Native
  (Nested (Nested (Single (transpose vss))))

-- For each Torch function, we can define a function like this which uses
-- Torch's implementation if there are no deferred calls. We also could avoid
-- writing a native instance by packing and then using the dynamic version.
dimNegate :: forall a ns. (Num a, DType a) => Dim ns a -> Dim ns a
dimNegate (Foreign t Id) = case dtype @a of
  DTypeDouble -> Foreign (dynamicNegate @Double t) Id
  DTypeInt -> Foreign (dynamicNegate @Int t) Id
dimNegate (Foreign t (Deferred f)) = Foreign t (Deferred (negate . f))
dimNegate (Native l) = Native (fmap negate l)

-- Packs first to force the use of Torch's implementation.
dimNegate' :: forall a ns. (Num a, DType a) => Dim ns a -> Dim ns a
dimNegate' = dimNegate . packForeign