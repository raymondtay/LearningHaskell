# Monad

Finally we come to one of the most talked about structures in Haskell: the monad. Monads are not, strictly speaking, necessary to Haskell.
Although the current standard for Haskell does use monad for constructing and transforming `IO` actions, older implementations of
Haskell did not. Monads are powerful and fun, but they do not define Haskell. Rather, monads are defined in terms of Haskell.

Monads are applicative functors, but they have something special about them that makes them different from and more powerful
than either `<*>` or `fmap` alone.

Here's how Monads are defined:
```haskell

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

```

Functor, Applicative and Monad instances for a given type should have the same core behavior. Its important to understand this
chain of dependency: 

Functor -> Applicative -> Monad

Whenever you have implemented an instance of Monad for a type you necessarily have an Applicative and a Functor as well.

# The novel part of Monad

Conventionally when we use monads, we use the bind function, `>>=`. Sometimes we use it diretly, sometimes 
indirectly via the `do` syntax. The question we should ask ourselves, is what's unique to Monad - at least
from the point of view of types?

We already saw that it's not return; that's just another name for pure from Applicative. We also noted that it
also isn't `>>` which has a counter part in Applicative. And it also isn't `>>=`, at least not in its entirety. The
type of >>= is visibly similar to that of fmap and <*>, which makes sense since monads are applicative functors. 
For the sake of making this maximally similar, we are going to changethe `m` of Monad to `f`:

The unique part of Monad is the following function:

```haskell

import Control.Monad (join)

join :: Monad m => m (m a) -> m a

-- compare with

concat :: [[a]] -> [a]

```

It is also somewhat novel that we can inject more structure via our function application, where applicatives
and fmaps have to leave the structure untouched. Allowing the function itself to alter the structure is something
we have not seen in Functor and Applicative, and we will explore the ramifications of that ability more, especially
when we start talking about the Maybe Monad. But we can inject more structure with a standard fmap if we wish. However,
the ability to flatten those two layers of structure into one is what truly makes Monad special. And it's by putting 
that `join` function together with the mapping function that we get `bind`, also known as `>>=`.

# What Monad is not

Monad is not:

+ Impure. Monadic functions are pure functions. `IO` is an abstract datatype that allows for impure, or effectful, actions
  and it has a `Monad` instance. But there's nothing impure about monads.

+ An embedded language for imperative programming. SPJ, one of the lead developers and researchers of Haskell
  and its implementation in GHC, has famously said: "Haskell is the world's finest imperative programming language"
  and hewas talking about the way monads handle effectful programming. While monads are often used for sequencing 
  actions in a way that looks like imperative programming, there are commutative monads that do not order actions.

