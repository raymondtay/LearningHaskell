There are a few terms of art around Monads that you may not be familiar with.
These aren't formal, but they're commonly used, so its helpful to know about
them:
- Monadic simply means "pertaining to Monads". A monadic type is an isntance of
  the Monad typeclass; a monadic value has a monadic type. 

- When we say that a type is a monad; this is really a shorthand way of saying
  that it's an instance of the Monad typeclass. Being an instance of Monad
  gives us the necessary monadic triple of type constructor, injection
  funciton, and chaining function.

- In the same way, a reference to "the Foo monad" implies that we are talking
  about the type named Foo and that it's an instance of Monad.

- An action is another name for a monadic value. This use of the word probably
  originated with the introduction of monads for I/O, where a monadic value
  such as print "foo" can have an observable side effect. A function with a
  monadic return type might also be inferred to as an action, though this is a
  little less common.


The `Monad` typeclass does not provide any means for values to escape their
monadic shackles. We can inject a value into a monad using `return`. We can
extract a value from a monad using `(>>=)` but the function on the right, which
can see an unwrapped value, has to wrap its own result back up agai.

Most monads have one or more `runLogger`-like functions. The notable exception
is of course `IO`, which we usually escape from simply by exiting a program.

A monad execution function runs the code inside the monad and unwraps its
result. Such functions are usually the only means provided for a value to
escape from its monadic wrapper. The author of a monad thus has complete
control over how whatever happens inside the monad gets out.


From the book "Real World Haskell" on page 328 reads
"""
We intentionally have said nothing about how the chaining and injection
functions of a monad should behave, because this almost does not matter. In
fact, monads are ubiquitous in Haskell code precisely because they are so
simple. Many common programming patterns have a monadic structure : passing
around implicit data or short-circuiting a chain of evaluations if one fails,
to choose but two.
"""

From the book "Real World Haskell" on page 334 reads
"""
Based on the code we have seen so far, monads seem to have a substantial
shortcoming:
the type constructor that wraps a monadic value makes it tricky to use a
normal, pure function on a value trapped inside a monadic wrapper.
"""

# Desugaring of `do`-Blocks

Haskell's `do` syntax is an example of syntactic sugar: it provides an
alternative way of writing monadic code, without using `(>>=)` and anonymous
functions. Desugaring is the translation of syntactic sugar back to the core
language.

The rules for desugaring a `do`-block are easy to follow. We can think of a
compiler as applying these rules mechanically and repeatedly to a do-block
until no more do keywords remain.

A do-keyword followed by a single action is translated to that action by
itself:
A do keyword followed by more than one action is translated to the first
action, then (>>), followed by a do keywrod and the remaining actions. When we
apply this rule repeatedly, the entire do-block ends up chained together by
applications of (>>).

# Monads and Functors

Functors and monads are closely related. The terms are borrowed from a branch
of mathematics called category theory, but they did not make the transition to
Haskell completely unscathed.

In category theory, a monad is built from a functor. You might expect that in
Haskell, the Monad typeclass would thus be a subclass of Functor, but it isn't
defined as such in the standard Prelude - an unfortunate oversight.

However, authors of Haskell libraries use a workaround: When programmers define
an instance of Monad for a type, they almost always write a Functor instance
for it, too. You can expect that you will be able to use the Functor
typeclass's `fmap` function with any monad.

# Separating Interface from Implementation

Another important way to make code more modular involves separating its
interface (What the code can do) from its implementation - how it does it.

The standard random number generator in `System.Random` is known to be quite
slow. One simple and effective way that we could deal with this is to provide
`Supply` with a better source of random numbers. Let's set this idea aside,
though, and consider an alternative approach,one that is useful in many
settings. We will separate the actions we can perform with the monad from how
it works using a typeclass:

```haskell
class (Monad m) => MonadSupply s m | m -> s where
  next :: m (Maybe s)
```

This typeclass defines the interface that any supply monad must implement. It
bears careful inspection, since it uses several unfamiliar Haskell language
extensions. We will cover each one in the sections that follow.

# The Reader Monad

The `State` monad lets us plumb a piece of mutable state through our code.
Sometimes, we would like to be able to pass some immutable state around, such
as a program's configuraiton data. We could use the `State` monad for this
purpose, but we might then find ourselves accidentally modifying data that
should remain unchanged.


