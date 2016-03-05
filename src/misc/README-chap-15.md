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

> foldr mappend mempty ([2,4,5]::[Product Int])
Product { getProduct = 40 }

> foldr mappend mempty ["blah", "blah"]
"blahblah"

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

# Why you might use newtype

- Signal intent: using `newtype` makes it clear that you only intend for
  it to be wrapper for the underlying type. The newtype cannot eventually
  grow into a more complicated sum or product type, while a normal
  datatype can.

- Improve type safety: avoid mixing up many values of the same representation
  such as `Text` and `Integer`.

- Add different typeclass instances to a type that is otherwise
  unchanged representationally.

# Laws 

Laws dictates what constitutes a valid instance or concrete instance of 
the algebra or set of operations we are working with. We care about the laws a 
Monoid must adhere to because mathematicians care about the laws. That matters
because mathematicians often want the same things programmers
want ! A proof that is inelegant, a proof term that doesn't compose well
or that cannot be understood is not very good or useful to a mathematician.

`Monoid` instances must abide by the following laws:

```haskell
mappend mempty x = x

mappend x mempty = x

mappend x (mappend y z) = mappend (mappend x y) z

mconcat = foldr mappend mempty
```

# The problem of orphan instances

We have said both in this chapter and in the earlier chapter devoted
to Typeclasses that typeclasses have unique pairings of the class and the
instance for a particular type.

We do sometimes end up with multiple instances for a single type when
orphan instances are written. But writing orphan instances should be avoided 
at all costs. Do not be lazy about this! if you get an orphan instance warning 
from GHC, fix it.

An orphan instance is when an instance is defined for a datatype and typeclass
but not in the same module as either the declaration of the typeclass
or the datatype. If you don't own the typeclass or the datatype, newtype it!

If you want an orphan instance so that you can have multiple instances for
the same type, you still want to use newtype. We saw this earlier with Sum
and Product which let us have notionally two Monoids for numbers without 
resorting to orphans or messing up typeclass instance uniqueness.

## Dealing with orphan issues

- You defined the type but not the typeclass ? Put the instance in the 
  same module as the type so that the type cannot be imported without 
  its instances.

- You defined the typeclass but not the type? Put the instance in the 
  same module as the typeclass definition so that the typeclass cannot
  be imported w/o its instances.

- Neither the type nor the typeclass are yours? Define your own newtype
  wrapping the original type and now you've got a type that
  "belongs" to you for which you can rightly define typeclass instances. 
  There are means of making this less annoying which we'll discuss later.

# Definitions

+ A monoid is a set that is closed under an associative binary operation
  and has an identity element. Closed is the posh mathematical way of saying it's
  type is :
```haskell
mappend :: Monoid m => m -> m -> m
```
such that your arguments and output will always inhabit the same type (set).

+ A semigroup is a set that is closed under an associative binary operation - and 
  nothing else.

+ Laws are rules about how an algebra or structure should behave. These are needed 
  in part to make abstraction over the commonalities of different instantiations of 
  the same sort of algeberate possible and practical. This is critical to having 
  abstractions which are not unpleasantly surprising.

+ When haskellers refer to algebras, they are usually talking abotu a somewhat
  informal notion of operations over a type and its laws, such as with semigroups
  monoids, groups, semirings, and rings.




