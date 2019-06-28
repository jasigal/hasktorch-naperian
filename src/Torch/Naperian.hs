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

module Torch.Naperian where

import           Naperian                hiding ( Pair, Hyper(..) )
import           Data.Naperian
import           Control.Applicative
import           Data.Functor.Product
import           Data.Functor.Compose
import           GHC.TypeLits
import           Data.Kind                      ( Type )

import qualified Torch.Tensor as D
import qualified Torch.Autograd as A
import qualified Torch.DType as TDT
import Torch.Static as S

data Reshape f g where
  Reshape :: (FiniteNaperian f, FiniteNaperian g) => (forall a. f a -> g a) -> Reshape f g

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

data Dim (fs :: [Type -> Type]) (ns :: [Nat]) (dtype :: TDT.DType) where
   Dim :: FiniteNaperian (FiniteHyper fs)
       => FiniteHyper fs (S.Tensor dtype ns) -> Dim fs ns dtype

instance Show (FiniteHyper fs String) => Show (Dim fs ns dtype) where
  show (Dim dim) = show (fmap show dim)

liftUnaryOp :: (D.Tensor -> D.Tensor) -> (Dim fs ns dtype -> Dim fs ns dtype)
liftUnaryOp op (Dim dim) = Dim $ fmap (UnsafeMkTensor . op) dyn
  where
    dyn = fmap toDynamic dim

liftBinOp :: (D.Tensor -> D.Tensor -> D.Tensor) -> (Dim fs ns dtype -> Dim fs ns dtype -> Dim fs ns dtype)
liftBinOp op (Dim dim1) (Dim dim2) = Dim . fmap UnsafeMkTensor $ liftA2 op dyn1 dyn2
  where
    dyn1 = fmap toDynamic dim1
    dyn2 = fmap toDynamic dim2

instance Num (Dim fs ns dtype) where
  (+) = liftBinOp (+)
  (-) = liftBinOp (-)
  (*) = liftBinOp (*)
  negate = liftUnaryOp negate
  abs = liftUnaryOp abs
  signum = liftUnaryOp signum
  fromInteger = error "unsupported"

foldDimLayer
  :: Foldable f
  =>(S.Tensor dtype ns -> S.Tensor dtype ns -> S.Tensor dtype ns)
  -> S.Tensor dtype ns
  -> Dim (f : fs) ns dtype
  -> Dim fs ns dtype
foldDimLayer reduce seed (Dim (Prism h)) = Dim $ fmap (foldr reduce seed) h

reshapeDim
  :: Log (FiniteHyper fs) ~ HList (MapLog fs)
  => Reshape f g -> Dim (f : fs) ns dtype -> Dim (g : fs) ns dtype
reshapeDim (Reshape nat) (Dim (Prism h)) = Dim (Prism (fmap nat h))
