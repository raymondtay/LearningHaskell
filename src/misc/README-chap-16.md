# Functors

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

`class` is the keyword to begin the definition of a typeclass.
`Functor` is the name of the typeclass we are defining
Typeclasses in Haskell usually refer to some sort of type. The letters
themselves, as with type variables in type signatures, do not mean anything
special. `f` is a conventional letter to choose when referring to types that
have functorial structure. The `f` must be the same `f` throughout the 
typeclass definition.

The `where` keyword ends the declaration of the typeclass name and associated
types. After the `where` the operations provided by the typeclass are listed.
We begin the declaration of an operation named `fmap`.
The argument `a -> b` is any function in haskell
The argument `f a` is a `Functor` `f` that takes a type argument `a`. that is
the `f` is `a` type that has an instance of the `Functor` typeclass.

The return value is `f b`. It is the same `f` from `f a`, while the type argument `b`
possibily but not necessarily refers to a different type.

# Functor Laws

Instances of the functor typeclass should abide by two basic laws.
Understanding these laws is critical for understanding Functors
and writing typeclass instances that are composable and easy to reason about.

## Identity

Basically, the following should hold for ANY functor you write:
```haskell
fmap id == id -- `id` is the identity function in the Prelude
```

## Composability

The law of composition can be represented by the following haskell code snippet:
```haskell
fmap (f . g) = fmap f . fmap g

-- examples
fmap( (+1) . (*2) ) [1,2,3]
[3,5,7]

-- which is equivalent to fmap (+1) . fmap (*2) $ [1,2,3]
```
If the implementation of `fmap` doesn't do this, it is a broken functor.
Both of these laws serve us by maintaining one critical condition: _structure preservation_.

## An example of :: Functor ::

```haskell
data WhoCares a = 
  ItDoesnt
  | Matter a
  | WhatIsThisCalled deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatIsThisCalled = WhatIsThisCalled
  fmap f (Matter x) = Matter (f x)
```
<pre>
Our instance must follow the identity law or else it is not a valid functor. 
That law dictates that `fmap id (Matter _ )` must not touch `Matter`.
That is, it must be identical to `id (Matter _)`. Functor is a way of
lifting over structure (mapping) in such a manner that you don't have to 
create about the structure because you are not _allowed_ to touch the
structure anyway.

It might be obvious to say this but it doesn't hurt to know that the identity
law is the law that prevents you from say `fmap _ ItDoesnt = WhatIsThisCalled`
and similarly `fmap _ WhatIsThisCalled = ItDoesnt`. :-)
</pre>

# How about an _real world_ example ? :: Fun ::

Let's consider an example to understand how to write _proper_ Functors.
```haskell

data Two a b = Two a b deriving (Eq, Show)
data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

```
Now, how does that work actually? The way to understand this is still by
examining the types in the following manner:
The expression `Two a` in the declaration of the Functor belongs to the Functor `f`
and the reason why this is the case is because if we examine the type of `fmap`
`fmap :: Functor f => (a -> b) -> f a -> f b`
and imagine the following mental map:
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
fmap :: (Two a)   => (b -> c) -> (Two a) b -> (Two a) c

-- The reason why f == Two a is because we have already declared
-- it so (see previous code snippet)
```
How about `data Or a b` ? Its basically the same semantics as the above.
Now in this situation, the `Or a` is already bound to the Functor `f` i.e.
the _first_ type of `Or` is bound to `f` => functor laws means that we cannot
alter it but we can do something about the second type of `Or` i.e. `b`.
```haskell 

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

```

# Definitions

+ A higher-kinded type is a type which itself takes types as arguments
  and potentially mentions them in the definition of the datatype.
  Functor is an example of higher kinded polymorphism because the kind
  of the `f` parameter to Functor is `* -> *`. Another example of higher
  kinded polymorphism would be a datatype having a parameter to the type
  constructor which is of a higher kind, such as 
  ```haskell
  data Weird f a = Weird (f a)
  ```
  Where the kinds of the types involved are:
  ```
  a :: *
  f :: * -> *
  Weird :: (* -> *) -> * -> *
  ```
  Here both `Weird` and `f` are higher-kinded, with 
  `Weird` being an example of higher-kinded polymorphism.


+ Functor is a mapping between categories. In Haskell, this manifests as
  a typeclass which lifts a function between two types over two new types.
  This conventionally implies some notion of a function which can be applied to
  a value with more structure than the unlifted function was originally designed for
  The addition structure is represented by the use of a higher-kined type f, introduced
  by the defintion of the Functor typeclass.
  ```haskell
  f :: a -> b
  fmap f :: f a -> f b
  -- f is applied to a single argument 
  -- and so is kind * -> *
  ```

  One should be careufl not to confuse the intuition for it necessarily
  being exclusively about containers or data structures. There's a Functor 
  of functions and many exotic types will have a lawful Functor instance.

