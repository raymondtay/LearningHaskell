module Chap16 where

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

