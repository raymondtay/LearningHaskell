module Chap16 where

data CountingBad a = Heisenberg Int a deriving (Eq, Show)
instance Functor CountingBad where
  fmap f (Heisenberg n a ) = Heisenberg (n) (f a) -- right !
  -- fmap f (Heisenberg n a ) = Heisenberg (n+1) (f a) wrong !

newtype Identity a = Identity a 

--
-- `Identity` is bound to the Functor `f`
-- i.e. fmap :: Functor f => (a -> b) -> f a -> f b
--      fmap :: Identity  => (a -> b) -> (Identity) a -> (Identity (a -> b) a)
--
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a

-- 
-- the types `a` and `a'` are essentially the same
-- but haskell disallows the same parameter to be used twice
--
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

-- 
-- @see the explanation for both Identity and Pair
--
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-- 
-- Section 16.11's short exercise
--
data Sum a b = 
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- 
-- This is special in the sense that the type parameter `b`
-- is a phantom-type which means that there is no corresponding
-- value/term on the RHS of the "="
-- 
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

-- 
-- For the Constant to be a valid Functor, it has to be
-- Constant a instead of Constant a b since the former is * -> * 
-- and the latter is *.
instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

data Wrap f a = Wrap (f a) deriving (Show, Eq)
instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

