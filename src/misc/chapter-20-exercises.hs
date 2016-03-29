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

