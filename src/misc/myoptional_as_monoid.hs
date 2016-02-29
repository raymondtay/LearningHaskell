-- Write the Monoid instance for our Maybe type renamed to Optional
-- Chapter 15 of the book
module MyOptional where

import Data.Monoid

data Optional a =
  Nada
  | Only a 
  deriving Show

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only x) = Only x
  mappend (Only x) Nada = Only x
  mappend (Only x) (Only y) = Only (mappend x y)
