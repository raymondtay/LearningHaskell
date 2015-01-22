module CoolFoldable where

{-

 We can use Monoids to help define folds over various data structures. 
 So far, we have only done folds over lists but lists are not the only data structure
 that can be folded over. We can define folds over almost any data structure.
 Trees, apparently lend themselves well to folding.

 Apparently, there's a typeclass called `Foldable` and found in `Data.Foldable`
 its best imported via `qualified`
-}

import qualified Data.Monoid as M
import qualified Data.Foldable as F

data Tree a = 
    Empty | 
    Node a (Tree a) (Tree a) deriving (Show)

{-
 class F.Foldable t where
   F.fold :: Monoid m => t m -> m
   F.foldMap :: Monoid m => (a -> m) -> t a -> m
   ...
-}

instance F.Foldable Tree where
    foldMap f Empty = M.mempty
    foldMap f (Node value left right) = 
        F.foldMap f left `M.mappend` f value `M.mappend` F.foldMap f right

{-

With the above typeclass declaration, we can now do the following :

> F.foldMap (\e -> e :[]) (Node 4 Empty Empty)
> [4]
> F.foldMap (\e -> e :[]) (Node 4 (Node 5 Empty Empty) Empty)
> [5,4]
> F.foldMap (\e -> e :[]) (Node 4 (Node 5 Empty (Node 6 Empty Empty)) Empty)
> [5,6,4]
> foldl (+) 0 $ F.foldMap (\e -> e :[]) (Node 4 (Node 5 Empty (Node 6 Empty Empty)) Empty)
> 15

-}

