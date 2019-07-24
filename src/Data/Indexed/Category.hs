{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Indexed.Category where

import Control.Category as C
import GHC.Generics ((:*:)(..))
import Data.Kind
import Data.Functor.Const
import Prelude (($))
import qualified Prelude as P

newtype Ix k a b = Ix {runIx :: forall (i :: k). a i -> b i}

instance Category (Ix k) where
  id = Ix P.id
  (Ix f) . (Ix g) = Ix (f P.. g)

ixAp :: Ix k a b -> a i -> b i
ixAp = runIx

class IxFunctor k (f :: (k -> Type) -> (k -> Type)) where
  ifmap :: Ix k a b -> Ix k (f a) (f b)

ifst :: Ix k (a :*: b) a
ifst = Ix $ \(a :*: _) -> a

isnd :: Ix k (a :*: b) b
isnd = Ix $ \(_ :*: b) -> b

iProductDist :: IxFunctor k f => Ix k (f (a :*: b)) (f a :*: f b)
iProductDist = Ix $ \p -> (ifmap ifst `ixAp` p) :*: (ifmap isnd `ixAp` p)

-- Need k to be used as a kind for GHC inference
class (IxFunctor k f, IxFunctor k g)
  => IxMergeable k (f :: (k -> Type) -> (k -> Type)) g where
  -- Merge the 'f a' and 'g b' by keeping the 'a' and the 'g'
  merge :: Ix k (f a :*: g b) (g a)

newtype IxFix k f (i :: k) = IxFix {unIxFix :: f (IxFix k f) i}

icata
  :: IxFunctor k f
  => Ix k (f a) a
  -> Ix k (IxFix k f) a
icata alg = alg . ifmap (icata alg) . Ix unIxFix

ixSpecToAlg
  :: IxMergeable k f g
  => Ix k (f (Const a)) (Const a :*: g (Const ()))
  -> Ix k (f (Const a :*: IxFix k g)) (Const a :*: IxFix k g)
ixSpecToAlg spec = Ix $ \level ->
  let fa :*: fgs = iProductDist `ixAp` level
      a  :*: g   = spec `ixAp` fa
  in a :*: (IxFix $ merge `ixAp` (fgs :*: g))

ixMapAccumAlg
  :: IxFunctor k f
  => Ix k (f (Const a :*: IxFix k g)) (Const a :*: IxFix k g)
  -> IxFix k f i
  -> (a, IxFix k g i)
ixMapAccumAlg alg f = let Const a :*: g = icata alg `ixAp` f in (a, g)

ixMapAccum
  :: IxMergeable k f g
  => Ix k (f (Const a)) (Const a :*: g (Const ()))
  -> IxFix k f i
  -> (a, IxFix k g i)
ixMapAccum spec = ixMapAccumAlg (ixSpecToAlg spec)
