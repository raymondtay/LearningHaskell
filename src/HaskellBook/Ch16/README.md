# Functors

## Functor is function application

In fact, `fmap` is a specific sort of function application. Let's look at the
types:
`fmap :: Functor f => (a -> b) -> f a -> f b`
There's also an infix operator for `fmap`. If you are using an older version of
GHC, you may need to import `Data.Functor` in order to use it in the REPL. Of
course, it has the same type as the prefix `fmap`:
`(<$>) :: Functor f => (a -> b) -> f a -> f b`

Notice the similarities:
`(<$>) :: Functor f => (a -> b) -> f a -> f b`
`($)   ::              (a -> b) -> f a -> f b`


## Typeclasses and constructor classes

You may have initially paused on the type construct `f` in the definition of
Functor having kind `* -> *` - this is quite natural! In fact, earlier versions
of Haskell didn't have a facility for expressing typeclasses in terms of
higher-kinded types at all. This was developed by Mark P. Jones while he was
working on an implementation of Haskell called `Gofer`.

# Functor Laws

Instances of the Functor typeclass should abide by two basic laws.
Understanding these laws is critical for understanding Functor and writing
typeclass instances that are composable and easy to reason about.

## Identity

The first law is the law of identity:
`fmap id == id`

basically, it says that if we fmap the identity function, it should have the
same result as passing our value to identity. 

## Composition 

The second law for Functor is the law of composition:
`fmap (f . g) == fmap f . fmap g`

This concerns the composability of fmap. If we compose two functions, f and g,
and fmap that over some structure, we should get the same result as if we
fmapped them and then composed them.

## Structure preservation

Both of these laws touch on the essential rule that functors must be structure
preserving. 

The point of Functor is to reify and be able to talk about cases where we want
to reuse functions in the presence of more structure and be transparently
oblivious to that additional structure. We already saw that in Functor is in
some sense just a special sort of function application, but since it is
special, we want to preserve the things about it that make it different and
more powerful than ordinary function application. So, we stick to the laws.


# Transforming the unapplied type argument 

We've seen that `f` must be a higher-kinded type and that Functor instances
must abide by two laws, and we have played around with some basic fmapping. We
know that the goal of fmapping is to leave the outer structure untouched while
transforming the type arguments inside.

Way back in the beginning, we noticed that when we fmap over a typle, it only
transforms the second argumnet .We saw a similar thing when we fmapped over and
either value.

# IO Functor

The `IO` type is an abstract data type, there are no data constructors that we
are permitted to pattern match on, so the typeclasses `IO` provides are the
only way you can work with values of type `IO a`.

What if we wanted to transform onlhy the structure and leave the type argument
to that structure or type constructor alone? With this, we have arrived at
natural transformations. We can attempt to put together a type to express what
we want.
























