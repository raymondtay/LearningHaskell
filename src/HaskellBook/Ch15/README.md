# Monoids and Semigroups !

A `Monoid` is a binary associative operation with an identity. This definition
alone tells you a lot if you are accustomed to picking apart mathematical
definitions. In plain english, Monoids are functions that takes two arguments
and follows two laws: associativity and identity. Associativity means the
arguments can be regrouped in different orders and give the same result, as in
addition. Identity means there exists some value such that when we pass it as
input to our function, the operation is rendered moot and the other value is
returned, such as when we add zero or multiply by one. `Monoid` is the
typeclass that generalizes these laws across types.

In Haskell, we think of types as having an instance of a typeclass. When we
represent abstract operations that can be reused across a set of types, we
usually represent them as a typeclass. For example, the typeclass `Monoid` is 
defined:

```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

