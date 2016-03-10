
# Difference btween fmap and `(<*>)`
```haskell
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

The difference appears to be quite small and innocuous.
We now have an f in front of our function (a -> b). But the
increase in power it introduces is profound. For one thing, any
Applicative also has a Functor and not merely by definition - 
you can define a Functor in terms of a provided 

Below is an an example of how a pair of Monoids can be defined:

```haskell

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  mappend (a, b) (a', b') = (mappend a a', mappend b b')

instance Monoid a => Applicative((,) a) where
  pure x = (mempty, x)
  (<*>) (a, b) (c, d) = (mappend a c, b d)

```
The point in question is really the following consideration :
While applicatives are really monoidal functors, be careful about taking it too 
literally. For one thing, Monoid and Applicative instances aren't rquired
or guaranteed to have the same monoid of struture, and the functorial
part may actually change things. Nevertheless, you might be able to 
see the implicit monoid in how the Applicative pattern matches on the 
Just and Nothing cases and compare that with this Monoid.

```haskell
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  mappend m Nothing = m
  mappend Nothing m = m
  mappend (Just a) (Just a') = Just (mappend a a')

instance Applicative Maybe where
  pure = Just
  Nothing (<*>) _ = Nothing
  _ (<*>) Nothing = Nothing
  Just f (<*>) Just g = Just (f g)
```

# Understanding List Applicatives 

We'll start with the list applicative because it is a clear way to get a sense
of the pattern. Let's start by specializing the types:

```haskell

(<*>) :: f  (a -> b) -> f  a -> f  b
(<*>) :: [] (a -> b) -> [] a -> [] b

pure :: a -> f  a 
pure :: a -> [] a 
```
Pushing forward on the examples, if we were to do the following:
```haskell
fmap (3^) [1..4]
[3,9,27,81]
``` and what that essentially means is that we map a function 
across all values of the given list. Turning the picture around a
and focusing on the list applicative, we are also mapping functions
over a bunch of values but this time around; we are mapping a plurality
of functions over a plurality of values:
```haskell
[(+1), (*2)] <*> [1..4]
2,3,4,5,2,4,6,8]
-- this makes a lot of sense when we break down the types:
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
listApply :: [(a -> b)] -> [a] -> [b]
listFmap :: (a -> b) -> [a] -> [b]
-- The f structure that is wrapped around our function in the listApply
-- function is itself a list. Therefore, our a -> b from Functor has become a 
-- list of a -> b

```
Hence, Applicatives maps each function value from the first list over the second list
applies the operations and returns one list. The fact that it doesn't return two lists
or a nested list or some other configuration in which both structures are preserved
is the monoidal part; the reason we don't have a list of functions merely 
concatenated with a list o values is the function application part.

As another example, we can see this relationship clearer  if we use the tuple
constructor with the list applicative. we will use the infix operator for fmap
to map the tuple constructor over the first list. This embeds an unapplied function
(the tuple data constructor in this case) into some structure (a list, in this case), and
returns a list of partially applied functions. The infix applicative will then apply
one list of operations to the second list, monoidally appending the two lists.
```haskell
Prelude> (,) <$> [1,2] <*> [3,4]
[(1,3),(1,4),(2,3),(2,4)]

```
Another way to do something "similar" is the following and remember to take some time
to understand what the code is actually doing ...
```haskell
Prelude> let add1 x = x + 1
add1 :: Num a => a -> a
Prelude> let mult2 x = x * 2
...
Prelude> [add1, mult2] <*> [1..4]
[2,3,4,5,2,4,6,8]
...
Prelude> [add1 . mult2] <*> [1..4]
[3,5,7,9]
...
Prelude> [(+1) . (*2)] <*> [1..4]
[3,5,7,9]

```


