# Applicative

In previous chapters, we have seen two common algebras that are used as
typeclasses in Haskell. Monoid gives us a means of mashing two values of the
same type together. Functor, on the other hand, is for function application
over some structure we don't want to have to think about. Monoid's core
operation, mappend smashes the structures together - when you mappend two
lists, they become one list, so the structures themselves have been joined.
However, the core operation of Functor, fmap applies a function to a value that
is within some structure whiel leaving that structure unaltered.

We come now to Applicative. _*Applicative is a monoidal functor*_. The applicative
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

# Identity

The `Identity` type here is a way to introduce structure without changing the
semantics of what you are doing. 

# Constant

This is not so different from the Identity type, except this not only provides
structure it also acts like the `const` function. 

# Applicative Laws

After examining the law, test each of the expressions in the REPL.

1. Identity

Here is the definition of the identity law:

pure id <*> v = v

as you may recall, Functor has a similar identity law, and comparing them
directly might help you see what's happening:

id [1..5]

fmap id [1..5]

pure id <*> [1..4]


The identity law states that all three of those should equal. You can test them
for equality in your REPL or you could write a simple test to get the answer.
So, what's `pure` doing for us? It's embedding our id function into some
strucutre so that we can use `apply` instead of `fmap`.

2. Composition

Here is the definition of the composition law for applicatives:

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

You may find the syntax a bit unusual and difficult to read here. This is
similar to the law of composition for Functor. It is the law stating that the
result of composing our functions first and then applying them and the result
of applying the functions first then composing them should be the same. We are
using the composition operator as a prefix instead of the more usual infix and
using `pure` in order to embed that operator into the appropriate structure so
that it can work with `apply`.

pure (.) <*> [(+1)] <*> [(*2)] <*> [1,2,4]

This law is meant to ensure that there are no surprises resulting from
composing your function applications.

3. Homomorphism 

A homomorphism is a structure preserving map between two categories. The effect
of applying a funciton that is embedded in some structure to a value that is
embedded in some structure should be the same as applying a function to a value
without affecting any outside structure:

pure f <*> pure x = pure (f x)

That's the statement of the law. Here's how it looks in practice:

pure (+1) <*> pure 1

pure ((+1) 1) 

Those two lines of code should give you the same result. In fact, the result
you see for those should be indistinguishable from the result of : 
(+1) 1 
because the structure that `pure` is providing there isn't really meaningful.
So you can thikn of this law as having to do with the monoidal part of the
applicative deal: the result should be the result of the function application
without doing anything other than combining the structure bits. Just as we saw
how `fmap` is really just a special type of function application that ignores a
context or surrounding strucutre, applicative is also function application that
preserves strucutre. However, with applicative, since the funciton being
applied also has a structure, the structures have to be monoidal and come
together in some fashion.

pure (+1) <*> pure 1 :: Maybe Int

pure((+1) 1) :: Maybe Int

Those two results should again be the same, but this time the structure is
being provided by Maybe, so will the result of :

(+1) 1

be equal this time around? 

Here are a couple more examples to try out:

pure (+1) <*> pure 1 :: [Int]

pure (+1) <*> pure 1:: Either a Int

# Either versus Validation

Often the interesting part of an Applicative is wherever the monoidal in
monoidal functor is coming from. One byproduct of this is that just as you can
have more than one valid Monoid for a given datatype, unlike Functor,
Applicative can have more than one valid and lawful instance for a given
datatype.

How does Validation differ? Principally in what the Applicative instance does
with errors. Rather than just short-circuiting when it has two error values, it
would use the Monoid typeclass to combine them. Often this will just be a list
or set of errors but you can do whatever you want.






# Definition

A Monad is a typeclass reifying an abstraction that is commonly used in Haskell. Instead of an ordinary function of type a to be,
you are functorially applying a function which produces more structure itself and using `join` to reduce the nested struture that 
results.

```haskell

fmap :: (a -> b) -> f a -> f b

(<*>) :: f (a -> b) -> f a -> f b

(=<<) :: (a -> f b) -> f a -> f b

```

A Monadic function is one which generates more structure after having been lifted over monadic structure. Contrast the function
arguments to fmap and >>= in

```haskell

fmap :: (a -> b) -> f a -> f b

(>>=) :: m a -> (a -> m b) -> m b

```
The significant difference is that the result is m b and requires joining the result after lifting the function over m.
What does this mean? That depends on the Monad instance. The distinction can be seen with ordinary function 
composition and kleisli composition as well:
```haskell

(.) :: (b -> c) -> (a -> b) -> a -> c

(>=>) :: Monad m => (a -> m b ) -> (b -> m c) -> a -> mc

```
