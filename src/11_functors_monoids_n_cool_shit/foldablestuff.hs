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

sometimes, i wonder how i ever learn haskell considering that i'm of average intelligence
but then again never doubt the important quality of perseverance and hardwork. of course
its a bonus when haskell helps you when you understand hte basics of it. So let's start
with an value like

Just (Just (*3))

we want to access the a -> b inside the Just Just ... and how do we do that?
one thing i remembered was that Maybe values are Functors and that means we can pass
a function to lift it out 

fmap (\f -> fmap (\g -> g 3) f) (Just (Just (*3)))

which returns us a Just (Just 9) which is pretty cool but at this point there's no value
to wrapping 2 Justs around the 9 so we want to flatten them and i remembered again 
that Maybe values are also Monads and we can use the `join` function on them
since the type signature is

join :: Monad n => m (m a) -> m a

and now we can write an expression like the following 

join $ fmap (\f -> fmap (\g -> g 3) f) (Just (Just (*3) ))

which gives us Just 9. Voila.

note: when i say "remembered" what i mean is that i ran ":i Maybe" which 
      returns a host of information and reminded me of what Maybe values are part of.

-}

