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

# Definitions

+ A higher-kinded type is a type which itself takes types as arguments
  and potentially mentions them in the definition of the datatype.

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

