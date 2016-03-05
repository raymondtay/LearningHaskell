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


