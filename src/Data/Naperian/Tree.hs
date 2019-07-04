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

module Data.Naperian.Tree where

import           Naperian
import           Data.Naperian
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Singletons.Prelude.Num

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
