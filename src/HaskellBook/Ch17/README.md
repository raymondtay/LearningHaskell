# Applicative

In previous chapters, we have seen two common algebras that are used as
typeclasses in Haskell. Monoid gives us a means of mashing two values of the
same type together. Functor, on the other hand, is for function application
over some structure we don't want to have to think about. Monoid's core
operation, mappend smashes the structures together - when you mappend two
lists, they become one list, so the structures themselves have been joined.
However, the core operation of Functor, fmap applies a function to a value that
is within some structure whiel leaving that structure unaltered.

We come now to Applicative. Applicative is a monoidal functor. The applicative
typeclass allows for functiona pplication lifted over structure (like
Functors), but with Applicative the function we are applying is also embedded
in some structure. Because the function and the value it's being applied to
both have structure, we have to smash those structures together. So,
Applicative involves Monoids and Functors. And that's a pretty powerful thing.
 
# Applicative functors are monoidal functors

```haskell

($)   ::   (a -> b) ->   a ->   b
(<$>) ::   (a -> b) -> f a -> f b
(<*>  :: f (a -> b) -> f a -> f b

```

We already know `$` to be something of a do-nothing infix function which exists
merely to give the right-hand side more precedence and thus avoid parentheses.
For our present purposes it acts as a nice proxy for ordinary function
application in its type.

When we get to <$>, the alias for fmap, we notice the first change is that we
are now lifting our (a -> b) over the f wrapped around our value and applying
the function to that value.

So how is Applicative a monodal functor actually? Can we actually demonstrate
this? Sure we can.

Recall that the Functor instance for the two-tuple ignores the first value
inside the tuple:

prelude> fmap (+1) ("nlah", 0)
("nlah", 1)

and this makes sense right? because of the definition

data (,) a b = ...
instance Functor ((,) a) where
  fmap f (a,b) = (a, f b)
...etc...


so the 'b' values are always being transformed. For the Applicative instance of
two-tuple, we don't need a Monoid for the 'b' because we are using function
application to produce the 'b'. However, for the first value in the tuple, we
still need the Monoid because we have two values and need to somehow turn that
into one value of the same type:

prelude> ("aaa", (+1)) <*> ("bbb", 3)
("aaabbb",4)

Notice that for the 'a' value, we didn't apply any function but they have
combined themselves as if by magic; that's due to the Monoid instance for the
'a' values. The 'b' values would follow the reasoning i gave you earlier which
explains why monoidal instances for 'b' values are never required.


```haskell

instance (Monoid a, Monoid b) => Monoid(a, b) where
  mempty = (mempty, mempty)
  (a, b) `mappend` (a',b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

```


