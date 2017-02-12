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

Integers form a monoid under summation and multiplication. We can similarly say
that lists form a monoid under concatenation.

It's worth pointing out here that numbers are not the only sets that have more
than one possible monoid. Lists have more than one possible monoid, although
for now we are only working with concatenation. We usually enforce the unique
instance rule by using `newtype` to separate the different monoidal behaviors.

A variant of monoid that provides even stronger guarantees is the abelian or
commutative monoid. Commutativity can be particularly helpful when doing
concurrent or distributed processing of data because it means the intermediate
results being computed in a different order would not change the eventual
answer.

Monoids are even strongly associated with the concept of folding or
catamorphism - something we do all the time in Haskell.
