{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Torch.Naperian where

import           Naperian                hiding ( Pair )
import           Data.Naperian
import           Control.Applicative
import           Data.Functor.Product
import           Data.Functor.Compose

type Reshape f g = forall a . f a -> g a

activation :: (Num a, Functor f) => (a -> a) -> f a -> f a
activation = fmap

addBias :: (Num a, Applicative f) => f a -> f a -> f a
addBias = liftA2 (+)

concatenate :: (Functor f, Functor g) => f a -> g a -> Product f g a
concatenate = Pair

window
  :: (Functor f, Functor g, Functor h)
  => (g a -> a)
  -> Reshape f (Compose h g)
  -> f a
  -> h a
window reduce spec input = fmap reduce . getCompose . spec $ input

convolve
  :: (Num a, Functor f, Dimension g, Functor h)
  => g a
  -> Reshape f (Compose h g)
  -> f a
  -> h a
convolve weights spec input = window (inner weights) spec input

full :: (Num a, Dimension f, Functor g) => f a -> Compose g f a -> g a
full input weights = inner input <$> getCompose weights
