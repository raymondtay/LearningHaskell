
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
# Identity 

The `Identity` type here is a way to introduce structure w/o changing the
semantics of what you are doing. We will see it used with these typesclasses
that involve function application around and over structure, but this type
itself isn't very interesting, as it has no semantic flavour.

##  Specializing the types

Here is what the type will look like when our structure is Identity:

```haskell
-- f -Identity
-- Applicative f =>

(<*>) :: f (a -> b ) -> f a -> f b
(<*>) :: Identity (a -> b) -> Identity a -> Identity b
```
The first question i would ask is why are we using Identity to introduce some structure?
What is the meaning of this ?

Let me try to answer this by writing instances of typeclasses 
so that we can make the expression 
```haskell
const <$> Identity [1,2,3] <*> Identity [1,2,2]
```
typecheck.
```haskell
newtype Identity a = Identity deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity g) = Identity (f g)

```

# Constant

This is not so different from the Identity type, except this not only 
provides structure it also acts like the `const` function. It sort of 
throws away a function application. If this seems confusing, it's because it is ;).
However, it is also something that, like Identity has real-life use cases, and you will see
it in other people's code. It can be difficult to get used to using it yourself
but we just keep trying.

This datatype is like the `const` function in that it takes two arguments
but one of them just gets discarded. In the case of the datatype, we have to 
map our function over the argument that gets discarded. So there is no 
value to map over, and the function application just doesn't happen.

## Specializing the types

All right, so here is what the types will look like:
```haskell

-- f - Constant a
(<*>) :: f ( a -> b ) -> f a -> f b
(<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b


pure :: a -> f a
pure :: a -> Constant e a

```




















