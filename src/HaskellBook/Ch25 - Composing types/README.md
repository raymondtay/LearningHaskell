# Composing Types

Functors and applicatives are both closed under composition : this means that
you can compose two functors (or two applicatives) and return another Functor
(or applicative, as the case may be). This is not true of monads, however, when
you compose two monds, the result is not necessarily another monad.

However, there are many times in "real code" when composing monads is
desirable. Different monads allows us to work with different effects. Composing
monads allows you to build up computations with multiple effects. By stacking,
for exampl, a Maybe monad with an IO, you can be performing IO actions while
also building up computations that have a possibility of failure, handled by
the Maybe Monad.

A monad transformer is a variant of an ordinary type that takes an
additional type argument which is assumed to have a monad instance. For
example, a MaybeT is the transformer variant of the Maybe type. The transformer
variant of a type gives us a Monad instance that binds over both bits of
structure. This allows us to compose monads and combine their effects. Getting
comfortable with monad transformers is important to becoming proficient in
Haskell, so we are going to take it pretty slowly and go step by step.

The important thing is that monad transformers are never sum or product types;
they are always just a means of wrapping one extra layer of structure around a
type; so there is never a reason they could not be newtypes.


# Monadic Stacking

Applicative allows us to apply functions of more than one argument in the
presence of functorial structure, enabling us to cope with this transition.

# IdentityT 

Just as `IdentityT` show off the most basic essence of Functor, Applicative and
Monad, `IdentityT` is going to help you begin to understand monad transformers.
Using this type that doesn't have a lot of interesting stuff going on with it
will help keep us focused on the types and the important fundamentals of
transformers. What we see here will be applicable to other transformers as
well, but types like Maybe and list introduce other possibilities (failure
cases, empty lists) that complicate things a bit.

```haskell

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show) 
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show) 

instance Functor Identity where
 fmap f (Identity a) = Identity (f a)

instance (Functor m) => Functor (IdentityT m) where
 fmap f (IdentityT g) = IdentityT (fmap f g)

```

The IdentityT instance here should look similar to the Functor instance for the
One datatype above - the `g` argument is the value inside the IdentityT with the
untouchable structure wrapped around it. 

Let's dissect the code and understand its individual moving parts.

```haskell

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity
  
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity (f a) 

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return :: a -> Identity a
  return = pure

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

```

## Type contructors are functions

Type constructors can take other type constructors as arguments, too, just as
functions can take other functions as arguments. This is what allows us to
compose types.

# Reason why we need Monad Transformers

According to the book, the reason why we need _monad transformers_ is because
of the failure to be able to compose two arbitrary monad types.

## No free burrito lunches

Since getting another Monad given the composition of two arbitrary types that
have a Monad instance is impossible, what can we do to get a Monad isntance
for combinations of types? The answer is, Monad Transformers.

# The essential extra of Monad Transformers

It may not seem like it, but the IdentityT monad transformer actually captures
the essence fo transformers generally. We only embarked on this quest because
we could not be guaranteed a monad instance given the composition of two types.
Given that, we know having Functor / Applicative / Monad at our disposal isn't
enough to make that new Monad instance.

"""
We needed to know one of the types concretely so that we could use runIdentityT
(essentially fmapping a fold of the IdentityT structure) and then repack the
value in IdentityT.
"""

# Finding a pattern

Transformers are bearers of single-type concrete information that let you
create ever bigger Monads in a sense. Nesting such as 
```haskell

(Monad m) => m ( m a ) 

```

is addressed by `join` already. WE use transformers when we want a >>=
operation over f and g of different types (but both have Monad instances). You
have to create new types called monad transformers and write Monad instances
for those types to have a way of dealing with the extra structure generated. 

The general pattern is this: You want to compose two polymorhpic types, f and
g, that each have a Monad isntance. But you will end up with this pattern:

```haskell

f( g (f b) )

```

Monad's bind cannot join those types, not with that intervening g. So you need
to get to this:

```haskell
f (f b) 
```

You won't be able to do that unless you have some way of folding the g in the
middle. You cannot do that with just Monad. The essence of Monad is `join`, but
here you have only one bit of `g` structure, not `g(g ...)` so that is not
enough. The straightforward thing to do is to make g concrete. With concrete
type information for the "inner" bit of structure, we can fold out the g and
get on with it. The good news is that transformers do not require f be
concrete; f can remain polymorphic so long as it has a Monad instance, so we
only write a transformer once for each type.

We can see this pattern with `IdentityT` as well. You may recall this step in
our process of writing `IdentityT`'s Monad.


