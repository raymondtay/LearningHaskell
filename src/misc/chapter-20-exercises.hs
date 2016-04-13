module Chap20 where

import Data.Monoid

data Identity a = Identity a 

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x
--
-- with foldr and foldl, we are doing basically the same thing 
-- but with the arguments swapped. We didn't need to do anything special
-- for foldMap - no need to make use of the Monoid instance for identity
-- since there is only one value, so we just apply the function
--
-- note that using foldMap still relies on the Monoid instance for the 
-- "a" value.


data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only x) = Only x
  mappend (Only x) Nada = Only x
  mappend (Only x) (Only y) = Only (mappend x y)

instance Foldable Optional where 
  foldr _ z Nada = z
  foldr f z (Only x) = f x z
  foldl _ z Nada = z
  foldl f z (Only x) = f z x
  foldMap _ Nada = mempty
  foldMap f (Only x) = f x

-- 
-- implement the following functions interms of foldMap or foldr
-- from Foldable, then try them out with multiple types that have Foldable instances.
--
sum :: (Foldable t, Monoid a) => t a -> a
sum = foldr mappend mempty

-- 
-- the standard implementation found in GHC 7.10.3
-- for "sum" and "product" are:
-- getSum . foldMap Sum
-- getProduct . foldMap Product
--
product :: (Foldable t, Monoid a) => t a -> a
product = foldr mappend mempty

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = any . ( == ) 

newtype Max a = Max { getMax :: Maybe a} deriving (Eq,Ord)
newtype Min a = Min { getMin :: Maybe a} deriving (Eq,Ord)
 
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend l (Max Nothing) = l
  mappend (Max Nothing) r = r
  mappend (Max a@(Just x)) (Max b@(Just y)) 
    | x >= y = Max a
    | otherwise = Max b

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend l (Min Nothing) = l
  mappend (Min Nothing) r = r
  mappend (Min a@(Just x)) (Min b@(Just y)) 
    | x <= y = Min a
    | otherwise = Min b
 
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = undefined 
-- minimum = Just . (foldr min 0) 

maximum :: (Foldable t, Num a, Ord a) => t a -> Maybe a
maximum = Just . (foldr max 0)

-- filter function for Foldable types. It is a bit silly, but it does
-- technically work. We need the Applicative so we have minimal 
-- structure to put a value into, then mappend with the rest of the structure.
--
filterF p = foldMap (\x -> if p x then pure x else mempty)


