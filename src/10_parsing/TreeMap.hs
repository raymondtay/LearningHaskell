module Tree where

import Prelude hiding (Functor) -- Hide the original functor defined in GHC.Base

data Tree a = Node (Tree a) (Tree a)
              | Leaf a
              deriving (Show)

treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b 
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

-- Page 245 of the book reads "Haskell provides a well known typeclass to further 
-- generalize `treeMap`. This typeclass is named Functor, and it defines one function, fmap:
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
  fmap = treeMap

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just v) = Just (f v)

