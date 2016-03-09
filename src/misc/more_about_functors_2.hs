{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

import GHC.Arr
import Test.QuickCheck

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

-- two helper functions to help us 
-- conduct the id and composition checks
--
idCheck x = (fmap id x) == x 
composeCheck f g x = fmap (f . g) x == (fmap f (fmap g x))

data Bool = False | True -- invalid functor
-- instance Functor FlipFunctor.Bool where -- invalid functor declaration since the kinds do not qualify
--   fmap _ FlipFunctor.True = FlipFunctor.True

data BoolAndSomethingElse a = False' a | True' a
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- idCheck' = quickCheck (idCheck :: BoolAndSomethingElse Int -> Prelude.Bool)
-- compositionCheck' = quickCheck (idCheck :: BoolAndSomethingElse Int -> Prelude.Bool)

data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- Use thekinds to guide u on this one, dont get too hung up on the
-- details.
--
newtype Mu f = InF { outF :: f (Mu f) }

newtype Mu' f = InF' { outF' :: f }
instance Functor Mu' where
  fmap f (InF' a) = InF' (f a)

-- It's important to differentiate between type- and data-constructors
-- in Haskell.
--
newtype Constant a b = Constant' { getConstant :: a } deriving (Show) -- 'b' is a phantom type
instance Functor (Constant a) where
  fmap f (Constant' a) = Constant' a

data D = D (Array Word Word) Int Int -- not possible since :k D is *

