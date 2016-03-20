# Monad

Finally we come to one of the most talked about structures in Haskell: the monad.
Monads are not strictly speaking, necessary to Haskell.
Although the current standard for Haskell does use monad for constructing
and transforming `IO` actions, older implementations of Haskell did not. 
Monads are powerful and fun, but they do not define Haskell. Rather, monads
are defined in terms of Haskell.

Monads are applicative functors, but they have something special about them
that makes them different from and more powerful than either `<*>` or `fmap` alone.

Let's take a deeper look at `Monad` by examining what makes a monad.
```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a

```
From the above, we make a realization that Monads are stronger than Applicatives,
which in turn are stronger than Functors; you can also derive Applicaative and 
Functor in terms of Monad, just as you can derive Functors in terms of Applicative.

What does this mean? It means you can write `fmap` using monadic operations
and it works just fine
```haskell
fmap f xs = xs >>= return . f

-- example is
[1..4] >>= return . (+1)
[2,3,4,5]

```














