{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}

module Torch.Naperian where

import           Naperian                hiding ( Pair
                                                , Hyper(..)
                                                )
import           Data.Naperian
import           Control.Applicative
import           Data.Foldable
import           Data.Functor.Product
import           Data.Functor.Compose
import           GHC.TypeLits
import           Data.Kind                      ( Type )
import           System.IO.Unsafe

import qualified Torch.Tensor                  as D
import qualified Torch.Autograd                as A
import qualified Torch.DType                   as TDT
import           Torch.Static                  as S

import qualified ATen.Managed.Native           as ATen
import qualified ATen.Managed.Cast             as ATen
import           ATen.Cast

data Reshape f g where
  Reshape :: (FiniteNaperian f, FiniteNaperian g)
          => (forall a. f a -> g a) -> Reshape f g

activation :: (Num a, Functor f) => (a -> a) -> f a -> f a
activation = fmap

addBias :: (Num a, Applicative f) => f a -> f a -> f a
addBias = liftA2 (+)

concatenate :: (Functor f, Functor g) => f a -> g a -> Product f g a
concatenate = Pair

{-# ANN window "HLint: ignore Eta reduce" #-}
window
  :: (FiniteNaperian f, FiniteNaperian g, FiniteNaperian h)
  => (g a -> a)
  -> Reshape f (Compose h g)
  -> f a
  -> h a
window reduce (Reshape spec) input = fmap reduce . getCompose . spec $ input

{-# ANN convolve "HLint: ignore Eta reduce" #-}
convolve
  :: (Num a, FiniteNaperian f, FiniteNaperian g, Dimension g, FiniteNaperian h)
  => g a
  -> Reshape f (Compose h g)
  -> f a
  -> h a
convolve weights spec input = window (inner weights) spec input

full :: (Num a, Dimension f, Functor g) => f a -> Compose g f a -> g a
full input weights = inner input <$> getCompose weights

data Dim (ns :: [Nat]) (fs :: [Type -> Type]) dtype where
   Dim :: FiniteNaperian (FiniteHyper fs)
       => FiniteHyper fs (S.Tensor dtype ns) -> Dim ns fs dtype

instance Show (FiniteHyper fs String) => Show (Dim ns fs dtype) where
  show (Dim dim) = show (fmap show dim)

liftUnaryOp :: (D.Tensor -> D.Tensor) -> (Dim ns fs dtype -> Dim ns fs dtype)
liftUnaryOp op (Dim dim) = Dim $ fmap (UnsafeMkTensor . op) dyn
  where dyn = fmap toDynamic dim

liftBinOp
  :: (D.Tensor -> D.Tensor -> D.Tensor)
  -> (Dim ns fs dtype -> Dim ns fs dtype -> Dim ns fs dtype)
liftBinOp op (Dim dim1) (Dim dim2) =
  Dim . fmap UnsafeMkTensor $ liftA2 op dyn1 dyn2
 where
  dyn1 = fmap toDynamic dim1
  dyn2 = fmap toDynamic dim2

instance Num (Dim fs ns dtype) where
  (+)         = liftBinOp (+)
  (-)         = liftBinOp (-)
  (*)         = liftBinOp (*)
  negate      = liftUnaryOp negate
  abs         = liftUnaryOp abs
  signum      = liftUnaryOp signum
  fromInteger = error "unsupported"

unbindDynamic :: D.Tensor -> [D.Tensor]
unbindDynamic t = unsafePerformIO $ (cast2 ATen.unbind_tl) t (0 :: Int)

unbind :: (KnownNat n)
       => S.Tensor dtype (n : ns) -> Vector n (S.Tensor dtype ns)
unbind t = fmap UnsafeMkTensor $ case fromList (unbindDynamic dyn) of
  Nothing -> error "unbind error"
  Just x -> x
  where
    dyn = toDynamic t

stackDynamic :: [D.Tensor] -> D.Tensor
stackDynamic [] = error "stack on empty list"
stackDynamic ts@(t : _) =
  if compatible
    then unsafePerformIO $ (cast2 ATen.stack_ll) ts (0 :: Int)
    else error "stack on incompatible tensors"
  where
    compatible = all (== D.shape t) (fmap D.shape ts)

stack :: KnownNat n => Vector n (S.Tensor dtype ns) -> S.Tensor dtype (n : ns)
stack ts = UnsafeMkTensor . stackDynamic . fmap toDynamic $ toList ts

catDynamic :: [D.Tensor] -> D.Tensor
catDynamic [] = error "cat on empty list"
catDynamic ts@(t : _) =
  if compatible && (D.shape t /= [])
    then unsafePerformIO $ (cast2 ATen.cat_ll) ts (0 :: Int)
    else error "cat on incompatible tensors"
  where
    compatible = all (== D.shape t) (fmap D.shape ts)

cat
  :: KnownNat m
  => Vector m (S.Tensor dtype (n : ns))
  -> S.Tensor dtype (m * n: ns)
cat ts = UnsafeMkTensor . catDynamic . fmap toDynamic $ toList ts

chunkDynamic :: D.Tensor -> Int -> [D.Tensor]
chunkDynamic t n = unsafePerformIO $ (cast3 ATen.chunk_tll) t n (0 :: Int)

chunk
  :: forall k m n ns dtype. (KnownNat k, (k * m) ~ n)
  => S.Tensor dtype (n : ns)
  -> Vector k (S.Tensor dtype (m : ns))
chunk t = case fromList (fmap UnsafeMkTensor chunks) of
  Nothing -> error "chunk error"
  Just ts -> ts
  where
    chunks = chunkDynamic (toDynamic t) (natValI @k)

dimUp :: FiniteNaperian f => Dim (Size f : ns) fs dtype -> Dim ns (f : fs) dtype
dimUp (Dim h) = case h of
  Scalar _ -> Dim . Prism $ fmap (fromVector . unbind) h
  Prism _ -> Dim . Prism $ fmap (fromVector . unbind) h

dimDown :: Dim ns (f : fs) dtype -> Dim (Size f : ns) fs dtype
dimDown (Dim (Prism h)) = Dim $ fmap (stack . toVector) h

dimCat :: Dim (n : ns) (f : fs) dtype -> Dim ((Size f) * n : ns) fs dtype
dimCat (Dim (Prism h)) = Dim $ fmap (cat . toVector) h

dimChunk
  :: (FiniteNaperian f, ((Size f) * m) ~ n)
  => Dim (n : ns) fs dtype
  -> Dim (m : ns) (f : fs) dtype
dimChunk (Dim h) = case h of
  Scalar _ -> Dim . Prism $ fmap (fromVector . chunk) h
  Prism _ -> Dim . Prism $ fmap (fromVector . chunk) h

fmapDim
  :: (S.Tensor dtype ns -> S.Tensor dtype' ns')
  -> (Dim ns fs dtype -> Dim ns' fs dtype')
fmapDim f (Dim h) = Dim (fmap f h)

foldrDimLayer
  :: (S.Tensor dtype' ns' -> S.Tensor dtype ns -> S.Tensor dtype ns)
  -> S.Tensor dtype ns
  -> Dim ns' (f : fs) dtype'
  -> Dim ns fs dtype
foldrDimLayer reduce seed (Dim (Prism h)) = Dim $ fmap (foldr reduce seed) h

foldr1DimLayer
  :: (S.Tensor dtype ns -> S.Tensor dtype ns -> S.Tensor dtype ns)
  -> Dim ns (f : fs) dtype
  -> Dim ns fs dtype
foldr1DimLayer reduce (Dim (Prism h)) = Dim $ fmap (foldr1 reduce) h

foldlDimLayer
  :: (S.Tensor dtype ns -> S.Tensor dtype' ns' -> S.Tensor dtype ns)
  -> S.Tensor dtype ns
  -> Dim ns' (f : fs) dtype'
  -> Dim ns fs dtype
foldlDimLayer reduce seed (Dim (Prism h)) = Dim $ fmap (foldl reduce seed) h

foldl1DimLayer
  :: (S.Tensor dtype ns -> S.Tensor dtype ns -> S.Tensor dtype ns)
  -> Dim ns (f : fs) dtype
  -> Dim ns fs dtype
foldl1DimLayer reduce (Dim (Prism h)) = Dim $ fmap (foldl1 reduce) h

traverseDim
  :: Applicative f
  => (S.Tensor dtype ns -> f (S.Tensor dtype' ns'))
  -> Dim ns fs dtype
  -> f (Dim ns' fs dtype')
traverseDim f (Dim h) = fmap Dim $ traverse f h

pureDim
  :: FiniteNaperian (FiniteHyper fs)
  => S.Tensor dtype ns
  -> Dim ns fs dtype
pureDim t = Dim (pure t)

liftA2Dim
  :: (S.Tensor dtype ns -> S.Tensor dtype' ns' -> S.Tensor dtype'' ns'')
  -> (Dim ns fs dtype -> Dim ns' fs dtype' -> Dim ns'' fs dtype'')
liftA2Dim f (Dim h) (Dim h') = Dim (liftA2 f h h')

reshapeDim
  :: Reshape f g
  -> Dim ns (f : fs) dtype
  -> Dim ns (g : fs) dtype
reshapeDim (Reshape nat) (Dim (Prism h)) =
  case h of
    Scalar _ -> Dim (Prism (fmap nat h))
    Prism _ -> Dim (Prism (fmap nat h))

pushDim :: FiniteNaperian f => f (Dim ns '[] d) -> Dim ns '[f] d
pushDim = Dim . Prism . Scalar . fmap (\(Dim (Scalar a)) -> a)

pullDim :: Dim ns '[f] d -> f (Dim ns '[] d)
pullDim (Dim (Prism (Scalar x))) = fmap (Dim . Scalar) x

viaNaperian
  :: forall f g ns ms fs gs d e
   . (FiniteNaperian f)
  => (Dim ns (f : fs) d -> Dim ms (g : gs) e)
  -> Dim (Size f : ns) fs d
  -> Dim (Size g : ms) gs e
viaNaperian f = dimDown . f . dimUp
