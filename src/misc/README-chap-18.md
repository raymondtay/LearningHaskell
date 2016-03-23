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
An interesting thing about Monads is the ability to order (i.e. chain) executions
together but before we go about doing that, let's understand how we think it _should_
work and then we'll take it from there ...

If we try to `fmap putStrLn` over `getLine`, it won't do anything and its instructive 
to understand why this is the case; once again, we invoke our type-hat and based on our
understanding on how things worked back then to try to see what's going on...

```haskell

getLine:: IO String
putStrLn :: String -> IO ()
-- the type we start with 
<$> :: Functor f => (a -> b) -> f a -> f b
-- our (a -> b) is putStrLn
            (a     -> b)
putStrLn :: String -> IO ()

```
So, `b` is specialized to `IO ()` which is going to jam another
IO action inside of the IO that getLine performs. What we need to do
is to order the executions using another mechanism and the question is
which function can we use ? Turns out the answer is `Control.Monad.join`

```haskell
> import Control.Monad (join)
> join $ putStrLn <$> getLine
blah 
blah
> :t join $ putStrLn <$> getLine
join $ putStrLn <$> getLine :: IO ()
```

As quoted in the book, 
<pre>
As it happens, the cleanest way to express "ordering" in a lambda 
calculus without bolting on something unpleasant is through nesting
of expressions or lambdas.
</pre>


# Monadic Laws

The Monad typeclass has laws, just as the other datatypes do.
These laws exist, as with all the other typeclass laws, to ensure
that your code does nothing surprising or harmful. If the Monad
isntance you write for your type abides by these laws, then your monads
should work as you want them to. To write your own instance, you only
have to define a `>>=` operation but you want your binding to be 
as predictable as possible.

## Identity Laws

Monad has two identity laws:

```haskell
m        >>= return = m   -- right identity
return x >>= f      = f x -- left identity
```

## Associativity

The law of associativity is not so different from other laws of associativity
we have seen. It does look a bit different beacuse of the nature of `>>=`:
```haskell
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

That is, re-grouping the functions should not have any impact on the final
result, same as the associativity of Monoid. 

