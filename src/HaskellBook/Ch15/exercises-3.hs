{-# LANGUAGE InstanceSigs #-}

module Chapter15_3 where

import Control.Monad
import qualified Data.Semigroup as S

-- Turns out <> exists both in Monoid and Semigroup
-- The exercises here relates to **Semigroups**
--

-- 
-- 12 (see below)
--
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success a)) <> (AccumulateRight (Success b)) = AccumulateRight (Success (a S.<> b))
  (AccumulateRight (Failure a)) <> _ = AccumulateRight (Failure a)

--
-- 11 (see below)
--
data Validation a b = Failure a | Success b deriving (Eq, Show)
instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Validation a b) where
  (Failure a) <> _ = Failure a
  (Success b) <> (Success c) = Success (b S.<> c)
  (Success b) <> _ = Success b

--
-- 10 (see below)
--
newtype Comp a = Comp { unComp :: (a -> a) }
instance S.Semigroup a => S.Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ (\x -> (f S.<> g) x)

-- 
-- 9  (see below)
--
newtype Combine a b = Combine { unCombine :: (a -> b) }
instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ (\x -> (f S.<> g) x)

--
-- 8 (see below)
--
data Or a b = Fst a | Snd b

showOr :: (Show a, Show b) => Or a b -> String
showOr (Fst a) = "Fst " ++ show a
showOr (Snd b) = "Snd " ++ show b

instance (Show a, Show b) => Show (Or a b) where
  show = showOr

instance S.Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> _ = Snd a 


