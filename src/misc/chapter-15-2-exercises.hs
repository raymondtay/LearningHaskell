module Chap15_2 where

{-
 - On Chapter 15's exercises on Semigroups, there's an interesting problem
 - which i'm working on involving Semigroups. I'm suppose to use Semigroups
 - and whatever type constraints i know of.
 -}

import Data.Monoid hiding ((<>))
import Data.Semigroup

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x )


r1 = let
  f = Combine $ \n -> Sum(n + 1)
  g = Combine $ \n -> Sum(n - 1) in
  unCombine (f <> g) $ 1

r2 = let
  f = Combine $ \n -> Sum(n + 1)
  g = Combine $ \n -> Sum(n - 1) in
  unCombine (g <> f) $ 1

r3 = let
  f = Combine $ \n -> Sum(n + 1)
  g = Combine $ \n -> Sum(n - 1) in
  unCombine (f <> f) $ 1

