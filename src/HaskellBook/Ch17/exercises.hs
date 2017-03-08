{-# LANGUAGE InstanceSigs #-}

module Chapter_17 where

import Data.Monoid

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

-- 
-- (<*>) :: f ( a -> b ) -> f a -> f b
-- wih the constraint that f is a Functor
-- so the construction of the expression for (<*>) is like this:
-- (1) i know its a function embedded in a functor, in this case Identity
--     and so i use the expression (Identity f) which is the same as (Identity
--     (a -> b))
-- (2) next thing i did is to provide a functor instance for Identity and then
--     the rest is quite straightforward w.r.t the reasons i gave for Functor.
--     See the previous exercises if i'm still confused.
--
instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)


-- 
-- This seems to be an example of a monoidal-functor.
--
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show) 

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance (Monoid a, Monoid b) => Monoid (Constant a b) where
  mempty = mempty
  mappend (Constant a) (Constant b) = Constant (a `mappend` b)

-- How did i arrive at this conclusion? Here's how.
-- 
-- 'pure' says that it returns a function like this : a -> f a
-- where 'f' is a Functor. By the previous definitions, i know that 'f' is both
-- a functor and monoid; so i apply 'a' (which i know to be monoid) to 'pure'
-- but i want the actual value embeded in the monoid context so i apply mempty
-- to 'a'.
--
-- Next, i look at (<*>) and then realize that 'a' is actually a function
-- embedded in the Constant context and since i know they are functors and
-- monoids, so i apply `mappend` to both .
instance Monoid a => Applicative (Constant a) where
  pure a = Constant (mempty a)
  (<*>) (Constant a) (Constant b) = Constant (mappend a b)

-- 
-- Copying from Haskell's inbuild data structure called `Maybe`
-- here is how i would declare a Maybe Applicative 
--
data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'
  (<*>) :: Maybe' ( a -> b ) -> Maybe' a -> Maybe' b
  (<*>) (Just' f) Nothing' = Nothing'
  (<*>) (Just' f) (Just' a) = Just' (f a)
  (<*>) (Nothing') _  = Nothing'

--
-- You normally would use an Applicative or craft of your own in the situation
-- where you are thinking : " I want to do something kinda like fmap, but my
-- function is embedded in the functorial structure too, not just the value i
-- want to apply my function to". This is the basic motivation of the
-- Applicative.
--

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil _ = Nil
  mappend _ Nil = Nil
  mappend (Cons a xs) ys = Cons a $ mappend xs ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a Nil) = Cons (f a) Nil
  fmap f (Cons ele tail) = Cons (f ele) (fmap f tail)

instance Applicative List where
  pure :: a -> List a
  pure a = Cons a Nil
  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) (Cons f b) ca = fmap f ca <> (b <*> ca)
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- 
-- Crafted this solution because i know fmap returns a `List a`
-- and typically `flatMap` returns a `List (List a)`; and i know concat
-- "flattens" a nested list, and hence the solution.
--
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

