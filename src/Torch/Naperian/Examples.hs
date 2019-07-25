{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Torch.Naperian.Examples where

import           Naperian               hiding (Pair(..), Hyper(..))
import qualified Naperian               as N
import           Data.Naperian
import           Torch.Naperian
import           Data.Functor.Compose
import           Data.Functor.Product
import           Data.Functor.Identity
import           Data.Functor.Classes
import           Data.Maybe
import           Numeric.AD
import           Data.Indexed.Category
import           Data.Naperian.Tree
import           GHC.TypeLits
import           Data.Traversable

import Torch.Static as S
import qualified Torch.Functions as D
import Torch.DType as DT

data Full f g a = Full {func :: a -> a, weights :: Compose g f a, biases :: g a}

layer :: (Num a, Dimension f, Dimension g) => Full f g a -> f a -> g a
layer (Full f w b) xs = f <$> ((+) <$> full xs w <*> b)

network
  :: Num a
  => Full (Vector 2) (Vector 4) a
  -> Full (Vector 4) (Vector 2) a
  -> Vector 2 a
  -> Vector 2 a
network l1 l2 = layer l2 . layer l1

type I = Identity

-- | Given @f@-shaped input @x@, we predict scalar output @y@ by storing an
-- @f@-shape of slopes @m@ and a bias @c@ for the model @y = (m . x) + c@.
newtype LinearModel f a = LinearModel {unLinearModel :: Product f I a}
  deriving (Show, Eq, Functor, Naperian)
deriving instance FiniteNaperian f => FiniteNaperian (LinearModel f)
deriving via (WrappedNaperian (LinearModel f)) instance
  (Naperian f) => Applicative (LinearModel f)
deriving via (WrappedFiniteNaperian (LinearModel f)) instance
  (FiniteNaperian f) => Foldable (LinearModel f)
instance (FiniteNaperian f) => Traversable (LinearModel f) where
  traverse = traverseFinNap
deriving via (WrappedFiniteNaperian (LinearModel f)) instance
  (FiniteNaperian f) => Dimension (LinearModel f)

-- | A sample of an @f@-shape input and a scalar output.
newtype Sample f a = Sample {unSample :: Product f I a}
  deriving (Show, Eq, Functor, Naperian)
deriving instance FiniteNaperian f => FiniteNaperian (Sample f)
deriving via (WrappedNaperian (Sample f)) instance
  (Naperian f) => Applicative (Sample f)
deriving via (WrappedFiniteNaperian (Sample f)) instance
  (FiniteNaperian f) => Foldable (Sample f)
deriving via (WrappedFiniteNaperian (Sample f)) instance
  (FiniteNaperian f) => Show1 (Sample f)
deriving via (WrappedFiniteNaperian (Sample f)) instance
  (FiniteNaperian f) => Eq1 (Sample f)
instance (FiniteNaperian f) => Traversable (Sample f) where
  traverse = traverseFinNap
deriving via (WrappedFiniteNaperian (Sample f)) instance
  (FiniteNaperian f) => Dimension (Sample f)

-- | A @g@-shape amount of @f@-shape samples.
newtype Samples g f a = Samples {unSamples :: Compose g (Sample f) a}
  deriving (Functor, Naperian)
deriving instance (FiniteNaperian f, FiniteNaperian g) => FiniteNaperian (Samples g f)
deriving instance (FiniteNaperian f, Show1 g) => Show1 (Samples g f)
instance (Show1 (Samples g f), Show a) => Show (Samples g f a) where
  showsPrec = showsPrec1
deriving instance (FiniteNaperian f, Eq1 g) => Eq1 (Samples g f)
instance (Eq1 (Samples g f), Eq a) => Eq (Samples g f a) where
  (==) = eq1
deriving via (WrappedNaperian (Samples g f)) instance
  (Naperian g, Naperian f) => Applicative (Samples g f)
deriving via (WrappedFiniteNaperian (Samples g f)) instance
  (FiniteNaperian g, FiniteNaperian f) => Foldable (Samples g f)
instance (FiniteNaperian g, FiniteNaperian f) => Traversable (Samples g f) where
  traverse = traverseFinNap

-- | Given samples and a model, calculate the MSE.
totalError
  :: forall a f g
   . (Num a, Dimension f, Dimension g)
  => Samples g f a
  -> LinearModel f a
  -> a
totalError (Samples (Compose samples)) (LinearModel model) = sum
  $ fmap errorAtSample samples
 where
  Pair slopes (Identity intercept) = model
  errorAtSample :: Sample f a -> a
  errorAtSample (Sample sample) =
    let Pair input (Identity output) = sample
    in  (output - (inner input slopes + intercept)) ^ 2

-- | Use gradient descent on MSE to produce a stream of more accurate models.
{-# ANN fitModel "HLint: ignore Eta reduce" #-}
fitModel
  :: (Fractional a, Ord a, Dimension f, FiniteNaperian f, Dimension g)
  => Samples g f a
  -> LinearModel f a
  -> [LinearModel f a]
fitModel samples start = gradientDescent (totalError (fmap auto samples)) start

-- | Example samples, essentially @[([0], 0), ([1], 1), ..., ([9], 9)]@ for
-- @(inputs, output)@.
samples :: Samples (Vector 10) (Vector 1) Double
samples = Samples . Compose $ fmap f viota
 where
  f :: Finite 10 -> Sample (Vector 1) Double
  f (Fin i) = let v = fromIntegral i :: Double in pure v

-- | Use 'fitModel' for 100 steps starting with the model @m = 0@ and @c = 0@.
fittedModel :: LinearModel (Vector 1) Double
fittedModel = fitModel samples (pure 0) !! 100

treeModule
  :: (a -> (b, c))
  -> (b -> b -> b)
  -> Tree s a
  -> (b, Tree s c)
treeModule leaf branch tree =
  let (b, ftree') = ixMapAccum (mkTreeSpec leaf branch) (treeToFix tree)
  in (b, fixToTree ftree')

treeModule'
  :: (a -> (b, c))
  -> (a -> b -> b -> (b, c))
  -> Tree' s a
  -> (b, Tree' s c)
treeModule' leaf branch tree =
  let (b, ftree') = ixMapAccum (mkTreeSpec' leaf branch) (treeToFix' tree)
  in (b, fixToTree' ftree')

data LSTMSpec (dIn :: Nat) (dOut :: Nat) d = LSTMSpec {
  forget :: (Dim '[dOut, dOut] '[] d, Dim '[dOut, dIn] '[] d, Dim '[dOut] '[] d),
  input  :: (Dim '[dOut, dOut] '[] d, Dim '[dOut, dIn] '[] d, Dim '[dOut] '[] d),
  output :: (Dim '[dOut, dOut] '[] d, Dim '[dOut, dIn] '[] d, Dim '[dOut] '[] d),
  update :: (Dim '[dOut, dOut] '[] d, Dim '[dOut, dIn] '[] d, Dim '[dOut] '[] d)
}

mv :: Tensor dtype '[n, k] -> Tensor dtype '[k] -> Tensor dtype '[n]
mv a b = UnsafeMkTensor $ D.matmul (toDynamic a) (toDynamic b)

linear
  :: S.All KnownNat [n, m]
  => Dim '[n, m] '[] d
  -> Dim '[m] '[] d
  -> Dim '[n] '[] d
linear = liftA2Dim mv

lstmModule
 :: S.All KnownNat '[dIn, dOut]
 => LSTMSpec dIn dOut d
 -> Dim '[dOut] '[N.Pair] d
 -> Dim '[dIn] '[] d
 -> (Dim '[dOut] '[N.Pair] d, Dim '[dOut] '[] d)
lstmModule LSTMSpec{..} prev xt = (cur, ht)
  where
    (ctp, htp) = let Dim (Prism (Scalar (N.Pair a b))) = prev
                 in (Dim $ Scalar a, Dim $ Scalar b)
    nn f (u, w, b) x h = liftUnaryOp f $ linear w x + linear u h + b

    it = nn D.sigmoid input  xt htp
    ft = nn D.sigmoid forget xt htp
    ot = nn D.sigmoid output xt htp
    ut = nn D.tanh    update xt htp
    ct = it * ut + ft * ctp
    ht = ot * liftUnaryOp D.tanh ct

    cur = let (Dim (Scalar vct), Dim (Scalar vht)) = (ct, ht)
          in Dim . Prism . Scalar $ N.Pair vct vht

lstm
  :: S.All KnownNat '[dIn, dOut, len]
  => LSTMSpec dIn dOut d
  -> Dim '[dIn] '[Vector len] d
  -> Dim '[dOut] '[Vector len] d
lstm spec (Dim (Prism (Scalar inputs))) =
  let (_, outputs) = mapAccumR (lstmModule spec) 0 (fmap (Dim . Scalar) inputs)
  in pushDim outputs