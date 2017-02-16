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

Commutativity is a strong property and can be useful in circumstances when you
might need to be able to reorder evaluation of your data for efficiency
purposes without needing to worry about the result changing. Distributed
systems use commutative monoids in designing and thinking about constraints,
which are monoids that guarantee their operation commutes.

Monoid abides by the law of associativity but not the law of commutativity,
even though some monoidal operations are commutative.

# The problem of orphan instances


Sometime, we do end up with multiple instances for a single type when orphan
instances are written. But writing orphan instances should be avoided at all
costs.
Do not be lazy about this! If you get an orphan instance warning from GHC, fix
it.

An orphan instance is when an instance is defined for a datatype and
typeclass,but not in the same module as either the declaration of the typeclass
or the datatype. If you don't own the typeclass or the datatype, newtype it!

If you want an orphan instance so that you can have multiple instances for the
same type, you still want to use newtype.

# Semigroup

```haskell

class Semigroup a where
  (<>) :: a -> a -> a
  
(a <> b) <> c = a <> (b <> c) 

```

Semigroup still provides a binary associative operation, one that typically
joins two things together (as in concatenation or summation) but doesn't have
an identity value. In that sense, its a _weaker_ algebra.

## NonEmpty a useful datatype

`NonEmpty` is defined as follows:

```haskell
data NonEmpty a = a :| [a] deriving (Eq, Show, Ord)
```

## Strength in an algebra can be weakness

When haskellers talk about the strength of an algebra, they usually mean the
number of operations it provides which in turn expands what you can do with any
given instance of that algebra without needing to know specifically what type
you are working with.

The reason we cannot and do not want to simply make all of our algebras as big
as possible is that there are datatypes which are very useful
representationally, but which do not have the ability to satisfy everything in
a larger algebra that could work fine if you removed an operation or law. This
becomes a serious problem if NonEmpty is the right datatype for something in
the domain you are representing. If you are an experienced programmer, think
carefully. How many times have you meant for a list to never be empty? 

To guarantee this and make the types more informative, we use types like
NonEmpty. The problem is that NonEmpty has no identity value for the combining
operation (`mappend`) in `Monoid`. So, we keep the associativity but drop the
identity value and its laws of left and right identity. This is what introduces
the need for and idea of Semigroup from a datatype.

Polymorphism isn't only useful for reusing code; it is also useful for
expressing intent through parametericity so that people reading the code know
what we meant to accomplish.

When Monoid is too strong or more than we need, we can use Semigroup. If you
are wondering what's weaker than Semigroup, the usual next step is removing the
associativity requirement, giving you a magma.

