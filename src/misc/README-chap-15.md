# Monoid

A monoid is a binary associative operatio with an identity.
In plain english, a monoid is a function that takes two arguments
and follows two laws: associativity and identity. Associativity
means the arguments can be regrouped (or reparenthesized or reassociated)
in different orders and give the same result, as in addition.

Identity means there exist some value such that when we pass it as input to 
our function, the operation is rendered moot and the other value is returned
such as when we add zero or multiply by one. Monoid is the typeclass that
generalizes these laws across types.

```haskell
> :browse Data.Monoid
(<>) :: Monoid m => m -> m -> m
newtype All = All {getAll :: Bool}
type role Alt representational nominal
newtype Alt (f :: k -> *) (a :: k) = Alt {getAlt :: f a}
newtype Any = Any {getAny :: Bool}
newtype Dual a = Dual {getDual :: a}
newtype Endo a = Endo {appEndo :: a -> a}
newtype First a = First {getFirst :: Maybe a}
newtype Last a = Last {getLast :: Maybe a}
newtype Product a = Product {getProduct :: a}
newtype Sum a = Sum {getSum :: a}
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```

and you can do things like this:

```haskell

> mconcat [(Product 2), (Product 45)]
Product {getProduct = 90}
> mconcat [Just(Product 2), Just(Product 45)]
Just (Product {getProduct = 90})

> mappend (Just(Product 2)) (Just(Product 45))
Just (Product {getProduct = 90})

> mempty :: Maybe (Sum Integer)
Nothing

```

## Why Integer doesn't have a Monoid

The type `Integer` does not have a `Monoid` instance. None of the
numeric types do. Yet it's clear that numbers have monoidal operations 
so what is up with that Haskell?


While in mathematics, the monoid of numbers is summation, there's
 not a clear reason why it cannot be multiplication. Both operations
are monoidal (binary, associative and having an identity value) but 
each type should only have one unique instance for a given typeclass, not 
two (one instance for a sum, one for a product)

