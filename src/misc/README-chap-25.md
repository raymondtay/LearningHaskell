# Composing Types

It's really about _Monad Transformers_ - the design philosophy, 
its principles and the practicalities of using them. For many 
programmers, monad transfomers are indistinguishable from _magic_,
so we want to approach them from both angles and demonstrate that they 
are both comprehensible via their types and quite practical in normal 
programming.

Functors and applicatives are both closed under composition: this
means that you can compose two functors (or two applicatives) And return 
another functor (or applicative). This is not true of Monads, however:
when you compose two monads, the result is not necessarily another Monad.

A Monad Transformer is a variant of an ordinary type that takes 
an additional type argument which is assumed to have a monad instance.
For example, _MaybeT_ is the transformer variant of the _Maybe_ type.
The transformer variant of a type gives us a _Monad_ instance that binds
over both bits of structure. This allows us to compose monads and combine
their effects. Gettig comfortable with monad transformers is important
to becoming proficient in Haskell, so we are going to take it pretty slowly
and go step by step. You won't necessarily want to start out early on defining
a bunch of transformer stacks yourself, but getting familiar is always advantageous.

The important thing is that Monad Transformers are never sum or product types;
they are always just a means of wrapping one extra layer of (monadic) structure 
around a type, so there is never a reason they couldn't be newtypes.

# Monad Transformers

The problem with _Monad Transformers_ is that you can put two 
together but you cannot get a new _Monad_ instance out of it. When we
need to get a new Monad instance, we need a monad transformer. It's not
magic and the answer is in the types.

We said above that a monad transformer is a type constructor that
takes a Monad as an argument and returns a Monad as a result. Note that 
the fundamental problem with composing two Monads lies in the 
impossibility of joining two unknown Monads. In order to make that join
happen, we need to reduce the polymorphism and get concrete information 
about one of the Monads that we are working with. The other Monad remains
polymorphic as a variable type argument to our type constructor. Transformers
help you make a monad out of multiple (2,3,4...) types that each have a 
Monad instance by wrapping around existing monads that provide each bit of
wanted functionality.

## Monadic Stacking 

Applicatives allows us to apply functions of more than one argument in the
presence of functorial structure, enabling us to cope with this transition.

Before we go there, let's take a step back and recap how things work when we
reflect upon an imaginary computation which we would call to allow us to 
lift a function that increments-4 to the underlying value that's embedded in a Applicative
structure.

Let's assume that applicative structure is `[Right 3]` which is of type `Num b => [Either a b]`
which means that the `right` value is a numeric type and let's for the purpose of 
demonstration, we want access to the underlying value which is the value `3` and increment 
that by 4 (i.e. end result is 7); this is how we would do it:

```haskell
*Chap25 System.Random Control.Applicative> [Right 3] >>= \x -> [Right (+4) <*> x]
[Right 7]
*Chap25 System.Random Control.Applicative> :i Monad
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
    -- Defined in ‘GHC.Base’
instance Monad m => Monad (WrappedMonad m)
  -- Defined in ‘Control.Applicative’
instance Monad (Either e) -- Defined in ‘Data.Either’
instance Monad [] -- Defined in ‘GHC.Base’
instance Monad Maybe -- Defined in ‘GHC.Base’
instance Monad IO -- Defined in ‘GHC.Base’
instance Monad ((->) r) -- Defined in ‘GHC.Base’
instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
```

Dissecting this computation ...

The notation `>>=` basically allowed us to `lift` the `[]` to reveal
`Right 3` which is captured in `x` and because we know that's a functor
and by definition we need a applicative functor to be able work thru it.
 
Now, the next question is whether is this understanding correct? Let's try it
out with a slightly more complex example since we know that `>>=` is like a lifting 
operator that removes 1-layer of abstraction on our behalf.

```haskell

*Chap25 System.Random Control.Applicative> [[Right 4]] >>= (\x -> x >>= (\y -> [Right(+4) <*> y]))
[Right 8]

```

Dissecting this computation again ....

The notation `>>=` is being applied twice here! and if we recall, we know that each time `>>=`
is being applied, it removes 1-layer for us and that means after two-applications, we would again
get the `Right 4` value and again we apply the `add-4-to-x` as you saw it previously.

However, did you notice something rather odd? We gave `>>= (\x -> x >>= (\y -> [Right(+4) <*> y]))`
the value `[[Right 4]]` and it returned a `[Right 8]` !!! What happened? This alluded back to the fact
that Monads are not _closed_ under operations.


## How do we do this in a generic manner?

So, the question becomes, how do we get one big bind over a type like the following ?
```haskell
IO (Reader String [a])
-- The following Monads are present in the above expression
-- IO, Reader and []
```

We could make one-off types for each combination, but this would be tiresome very
quickly. For example:
```haskell
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a = MaybeList { runMaybeList :: [Maybe a] }
```
We do not need to resort to this: we can get a Monad for `two` types, as long as we know
what one of those types is. Transformers are a means of avoiding making a one-off Monad
for every possible combination of types.

### IdentityT 

`IdentityT` is going to help us onboard on _monad transformers_.
```haskell

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a )

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

```

The Identity instance should be familiar. In the IdentityT instance, the `fab` variable
represents the `f (a -> b)` that is the first argument of `(<*>)`. Since this can rely on the
Applicative instance for `m` to handle that bit, this instance defines how to 
applicatively apply in the presence of that outer IdentityT layer.




