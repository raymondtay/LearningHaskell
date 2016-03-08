{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

-- Functors in Haskell are unique for a 
-- given datatype
--
-- In haskell, Functor instances will be unique for a given 
-- datatype. We saw that this isn't true for Monoid; however,
-- we use newtypes to avoid confusing different Monoid instances 
-- for a given type. But Functor instances will be unique for a 
-- datatype, in part because of parametricity, in part because arguments
-- to type constructors are applied in order of definition. In a
-- hypothetical not-Haskell language, the following might be possible:

data Tuple a b = Tuple a b deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

data Bool = False | True

data BoolAndSomethingElse a = False' a | True' a

data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- Use thekinds to guide u on this one, dont get too hung up on the
-- details.
newtype Mu f = InF { outF :: f (Mu f) }

import GHC.Arr
data D = D (Array Word Word) Int Int


