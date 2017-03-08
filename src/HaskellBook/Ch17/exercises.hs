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
  mappend Nil xs = xs
  mappend xs Nil = xs
  mappend (Cons a xs) ys = Cons a $ mappend xs ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a Nil) = Cons (f a) Nil
  fmap f (Cons ele tail) = Cons (f ele) (fmap f tail)

--
-- How did i arrive at this declaration? This deserves a good analysis because
-- i was confused ... question was what was i really confused by?
--
-- The one problem i had was the fact that when i have the situation of 
--
-- (Cons f somelist) somelist' 
--
-- and i didn't know how was i suppose to apply the function 'f' to both
-- somelist and somelist' and finally merging them??? Finally, i realize we can
-- leverage the Monoid's mappend (i.e. <>) to do this and hence i went about
-- making List an instance of the Monoid (see above the definition). Next thing
-- i had to do was to actually merge it. Question is how??
--
-- At this point in time, i was confused by the symbolic representation i.e.
-- List (a -> b) and i began to misinterpret it ... long story short the
-- correct way to interpret is as follows:
-- 
-- Cons f Nil => which means a List of 1 function
-- Cons f tail => which means a list of n-functions since we have no idea how
-- many functions are in `tail`. 
--
-- After correcting myself, i realize the best way and infact the correct way
-- to understand this is to realize that `tail` is infact List(a -> b).
-- Therefore:
--
-- (<*>) (Cons f fs) ca = something <> something'
--
-- something = fmap f ca since ca is a `List a`
-- something' = (fs <> ca) since fs is `List(a->b)` and ca is `List a` which
-- fits the Applicative use case and finally we have the expression:
--
-- (<*>) (Cons f fs) ca = fmap f ca <> (fs <*> ca) 
--
instance Applicative List where
  pure :: a -> List a
  pure a = Cons a Nil
  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) (Cons f fs) ca = fmap f ca <> (fs <*> ca)
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

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons h tail)
  | n >  0 = Cons h (take' (n - 1) tail)
  | n <= 0 = Nil

--
-- The definitions i have below are mostly implemented from the book and the
-- thing i had to declare was for the `Monoid` instances for ZipList'; reason
-- for doing that was because i wanted to reuse the new swanky technique i
-- learnt about leveraging monoids to merge the results of the Applicative
-- application.
--
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Monoid (ZipList' a) where
  mempty = ZipList' Nil
  mappend (ZipList' xs) (ZipList' ys) = ZipList' $ xs `mappend` ys

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure :: a -> ZipList' a
  pure a = ZipList' (Cons a Nil)
  (<*>) :: ZipList'(a -> b) -> ZipList' a -> ZipList' b
  (<*>) (ZipList' Nil) (ZipList' xs) = ZipList' Nil
  (<*>) (ZipList' (Cons fs xs)) (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons fs xs)) (ZipList' ys) = ZipList' $ fmap fs ys <> (xs <*> ys)

