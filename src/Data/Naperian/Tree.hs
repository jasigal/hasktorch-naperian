{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Naperian.Tree where

import           Naperian
import           Data.Naperian
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Singletons.Prelude.Num
import           Data.Kind

$(singletons [d|
  data TShape where
    TUnit :: TShape
    TOp :: TShape -> TShape -> TShape

  deriving instance Eq TShape

  tSize :: TShape -> Nat
  tSize TUnit = 1
  tSize (TOp l r) = (tSize l) + (tSize r)
  |])

deriving instance Show TShape

data TIndex (s :: TShape) where
  TNil :: TIndex TUnit
  TLeft :: TIndex s1 -> TIndex (TOp s1 s2)
  TRight :: TIndex s2 -> TIndex (TOp s1 s2)

instance SingI s => Bounded (TIndex s) where
  minBound = case sing @s of
    STUnit     -> TNil
    STOp sl sr -> withSingI sl $ TLeft minBound
  maxBound = case sing @s of
    STUnit     -> TNil
    STOp sl sr -> withSingI sr $ TRight maxBound

instance SingI s => Enum (TIndex s) where
  fromEnum t = case sing @s of
    STUnit     -> 0
    STOp sl sr -> case t of
      TLeft t' -> withSingI sl $ fromEnum t'
      TRight t' ->
        withSingI sr $ (fromIntegral $ fromSing (sTSize sl)) + fromEnum t'
  toEnum i = case sing @s of
    STUnit -> if i == 0 then TNil else error "toEnum: out of bounds"
    STOp sl sr ->
      let leftSize = fromIntegral $ fromSing (sTSize sl)
      in  if i < leftSize
            then withSingI sl $ TLeft $ toEnum i
            else withSingI sr $ TRight $ toEnum (i - leftSize)

deriving instance Eq (TIndex s)
deriving instance Show (TIndex s)

data Tree (s :: TShape) a where
  TLeaf :: a -> Tree TUnit a
  TBranch :: Tree r a -> Tree l a -> Tree (TOp r l) a

deriving instance Show a => Show (Tree s a)
deriving instance Eq a => Eq (Tree s a)
deriving instance Functor (Tree s)

instance SingI s => Naperian (Tree s) where
  type Log (Tree s) = TIndex s
  lookup t i = case sing @s of
    STUnit -> case (t, i) of
      (TLeaf x, TNil) -> x
    STOp sl sr -> case t of
      TBranch l r -> case i of
        TLeft  i' -> withSingI sl $ Naperian.lookup l i'
        TRight i' -> withSingI sr $ Naperian.lookup r i'
  positions = case sing @s of
    STUnit     -> TLeaf TNil
    STOp sl sr -> withSingI sl $ withSingI sr $ TBranch
      (fmap TLeft positions)
      (fmap TRight positions)

deriving via (WrappedNaperian (Tree s)) instance
  SingI s => Applicative (Tree s)

instance (SingI s, KnownNat (TSize s)) => FiniteNaperian (Tree s) where
  type Size (Tree s) = TSize s

instance SingI s => Foldable (Tree s) where
  foldMap f t = case sing @s of
    STUnit -> case t of
      TLeaf x -> f x
    STOp sl sr -> case t of
      TBranch l r -> withSingI sl (foldMap f l) <> withSingI sr (foldMap f r)

deriving via (WrappedFiniteNaperian (Tree s)) instance
  (SingI s, KnownNat (TSize s)) => Dimension (Tree s)

-- Same as WrappedFiniteNaperian instance, written for clarity
instance SingI s => Traversable (Tree s) where
  traverse f t = case sing @s of
    STUnit -> case t of
      TLeaf x -> TLeaf <$> f x
    STOp sl sr -> case t of
      TBranch l r ->
        TBranch <$> withSingI sl (traverse f l) <*> withSingI sr (traverse f r)

data TreeS a (f :: TShape -> Type) (s :: TShape) where
  TLeafS :: a -> TreeS a f TUnit
  TBranchS :: a -> f l -> f r -> TreeS a f (TOp l r)

newtype SFix f s = SFix {unSFix :: f (SFix f) s}

-- Indexed types form a cartesian closedcategory

-- Homset
type f :-> g = forall s. f s -> g s
-- Exponential
newtype (:=>) f g s = Exp {unExp :: f s -> g s}

-- Internalize morphism
sinternal :: (f :-> g) -> (SConst () :-> (f :=> g))
sinternal func _ = Exp func

-- Externalize morphism
sexternal :: (SConst () :-> (f :=> g)) -> (f :-> g)
sexternal int = let Exp func = int (SConst ()) in func

-- Eval
seval :: ((f :=> g) :*: f) :-> g
seval (P func val) = unExp func val

-- Internal curry
sicurry :: ((f :*: g) :=> e) :-> (f :=> (g :=> e))
sicurry (Exp func) = Exp $ \f -> Exp $ \g -> func (P f g)

-- Internal uncurry
siuncurry :: (f :=> (g :=> e)) :-> ((f :*: g) :=> e)
siuncurry (Exp fge) = Exp $ \(P f g) -> let Exp ge = fge f in ge g

-- External curry
securry :: ((f :*: g) :-> e) -> (f :-> (g :=> e))
securry func f = Exp $ \g -> func (P f g)

-- External uncurry
seuncurry :: (f :-> (g :=> e)) -> ((f :*: g) :-> e)
seuncurry fge (P f g) = let Exp ge = fge f in ge g

class ShapeFunctor (h :: (TShape -> Type) -> (TShape -> Type)) where
  sfmap :: (f :-> g) -> (h f :-> h g)

class ShapeFunctor h => ShapeApply h where
  sdist :: (h f :*: h g) :-> h (f :*: g)
  -- sap :: h (f :=> g) :-> (h f :=> h g)
  -- sap = securry (sfmap seval . sdist)

type h :~> k = forall s f. h f s -> k f s

class (ShapeFunctor h, ShapeFunctor h') => Patchable h h' where
  patch :: forall s f g. h f s -> h' g s -> h' f s

instance ShapeFunctor (TreeS a) where
  sfmap f (TLeafS a) = TLeafS a
  sfmap f (TBranchS a t1 t2) = TBranchS a (f t1) (f t2)

instance Monoid a => ShapeApply (TreeS a) where
  sdist (P (TLeafS af) (TLeafS ag)) = TLeafS (af <> ag)
  sdist (P (TBranchS af t1f t2f) (TBranchS ag t1g t2g)) =
    TBranchS (af <> ag) (P t1f t1g) (P t2f t2g)

-- Replace f by g in left h
replace :: ShapeApply h => (h f :*: h g) :-> h g
replace = sfmap proj2 . sdist

instance Patchable (TreeS a) (TreeS b) where
  patch (TLeafS _) (TLeafS a) = (TLeafS a)
  patch (TBranchS _ f1 f2) (TBranchS b _ _) = TBranchS b f1 f2

scata
  :: ShapeFunctor h
  => (h f :-> f)
  -> (SFix h :-> f)
scata alg = alg . sfmap (scata alg) . unSFix

newtype SConst a s = SConst {unSConst :: a}
instance Show a => Show (SConst a s) where
  show (SConst a) = "SConst " ++ show a

data (f :*: g) s = P (f s) (g s)
  deriving Show

proj1 :: (f :*: g) :-> f
proj1 (P f _) = f

proj2 :: (f :*: g) :-> g
proj2 (P _ g) = g

shapeDist :: ShapeFunctor h => h (f :*: g) :-> (h f :*: h g)
shapeDist x = P (sfmap proj1 x) (sfmap proj2 x)

treeSpec
  :: (a -> (b, c))
  -> (a -> b -> b -> (b, c))
  -> (TreeS a (SConst b)) :-> (SConst b :*: TreeS c (SConst ()))
treeSpec f g (TLeafS a) = let (b, c) = f a in P (SConst b) (TLeafS c)
treeSpec f g (TBranchS a (SConst b1) (SConst b2)) =
  let (b, c) = g a b1 b2 in P (SConst b) (TBranchS c (SConst ()) (SConst ()))

treeAlg
  :: (a -> (b, c))
  -> (a -> b -> b -> (b, c))
  -> (TreeS a (SConst b :*: SFix (TreeS c)) :-> (SConst b :*: SFix (TreeS c)))
treeAlg f g (TLeafS a) = let (b, c) = f a in P (SConst b) (SFix $ TLeafS c)
treeAlg f g (TBranchS a p1 p2) =
  let P (SConst b1) t1 = p1
      P (SConst b2) t2 = p2
      (b, c) = g a b1 b2
  in P (SConst b) (SFix $ TBranchS c t1 t2)

treeSpecToAlg
  :: TreeS a (SConst b) :-> (SConst b :*: TreeS c (SConst ()))
  -> TreeS a (SConst b :*: SFix (TreeS c)) :-> (SConst b :*: SFix (TreeS c))
treeSpecToAlg spec level =
  case shapeDist level of
    P bs cs ->
      let P b c = spec bs
      in P b (SFix $ patch cs c)

sMapAccumTree
  :: (TreeS a (SConst b :*: SFix (TreeS c)):-> (SConst b :*: SFix (TreeS c)))
  -> SFix (TreeS a) s
  -> (b, SFix (TreeS c) s)
sMapAccumTree alg h = let P (SConst b) tc = scata alg h in (b, tc)

specToAlg
  :: Patchable h h'
  => h (SConst b) :-> (SConst b :*: h' (SConst ()))
  -> h (SConst b :*: SFix h') :-> (SConst b :*: SFix h')
specToAlg spec level =
  case shapeDist level of
    P bs cs ->
      let P b c = spec bs
      in P b (SFix $ patch cs c)

sMapAccumAlg
  :: ShapeFunctor h
  => (h (SConst b :*: SFix h'):-> (SConst b :*: SFix h'))
  -> SFix h s
  -> (b, SFix h' s)
sMapAccumAlg alg h = let P (SConst b) h' = scata alg h in (b, h')

sMapAccumSpec
  :: Patchable h h'
  => h (SConst b) :-> (SConst b :*: h' (SConst ()))
  -> SFix h s
  -> (b, SFix h' s)
sMapAccumSpec spec = sMapAccumAlg (specToAlg spec)
