# Monad

Finally we come to one of the most talked about structures in Haskell: the monad.
Monads are not strictly speaking, necessary to Haskell.
Although the current standard for Haskell does use monad for constructing
and transforming `IO` actions, older implementations of Haskell did not. 
Monads are powerful and fun, but they do not define Haskell. Rather, monads
are defined in terms of Haskell.

## What Monad is not ?

Since Monad is somewhat abstract and can be quite slippery, many 
people talk about it from one or two perspectives that they feel most
comfortable with. Quite often, they address what `Monad` is from the 
perspective of the `IO Monad`. `IO` does have a `Monad` instance 
and it is a very common use of monads. However, understanding monads only
through that instance leads to limited intuitions for what monads are 
and can do, and to a lesser extent, a wrong notion of what `IO` is all
about.

Monad is not:

+ Impure. Monadic functions are pure functions. Io is an abstract
  data type that allows for impure, or effectful, actions and it has
  a Monad instance. But there is nothing impure about monads.

+ An embedded language for imperative programming. Simon Peyton Jones,
  one of the lead developers and researchers of Haskell and its implementataion
  in GHC, has famously said, "Haskell is the world's finest imperative programming
  language", and he was talking about the way monad handle effectful programming.
  While monads are often used for sequencing actions in a way that looks like
  imperative programming, there are commutative monads that do not order actions.

+ A value. The typeclass describes a specific relationship between 
  elements in a domain and defines some operations over them.
  When we refer to something as "a monad", we are using that the same way
  we talk about "a monoid" or "a functor". None of theese are values.

+ About strictness. The monadic operations of bind and return are nonstrict. 
  Some operations can be made strict within a specific instances.

Using Monads does not require math. Or category theory.


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

--
-- Here's how it looks like and that is to say 
-- the IO () is suppressed as its nested inside a IO.
-- 
putStrLn <$> getLine :: IO (IO ())

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

What `join` did here is to _merge_ the effects of `getLine` and `putStrLn` 
into a single `IO` action. This merged IO action performs the effects in the 
`order` determined by the nesting of the `IO` actions. As it happens, the cleanest
way to express "ordering" in a lambda calculus without bolting on something
unpleasant is through nesting of expressions or lambdas.


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

# Notes

## What is a Monad ?

A monad is a typeclass reifying an abstraction that is commonly
used in Haskell. Instead of an ordinary function of type a to b
you are functorially applying a function which produces more 
structure itself an dusing join to reduce the nested structure that
results.
```haskell
fmap :: (a -> b ) -> f a  -> f b
(<*>) :: f (a -> b) -> f a -> f b
(=<<) :: (a -> f b) -> f a -> f b
```

## What is a Monadic function?

A monadic function is one which generates more structure after having
been lifted over monadic structure. Contrast the function arguments to 
`fmap` and `(>>=)` in:
```haskell
fmap :: (a -> b) -> f a -> f b
(>>=) :: m a -> (a -> m b) -> m b
```
The significant difference is that the result is `m b` and requires
`join`-ing the result after lifting the function over `m`. What does this mean?
That depends on the Monad instance.
The distinction can be seen as ordinary function composition and kleisli
composition as well:
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> mc
```

## What is _bind_ ? 

`bind` is unfortunately a somewhat overloaded term. You first saw it used early
in the book with respect to binding variables to values, such as with the
following:

```haskell
let x = 2 in x + 1
```
where x is a variable bound to 2. However, when we are talking about
a Monad instance typically bind will refer to having
used >>= to lift a monadic function over the structure.
The distinction being:
```haskell
-- lifting (a -> b) over f in f a
fmap :: (a -> b) -> f a -> f b

-- binding (a -> m b) over m in m a
(>>=) :: m a -> (a -> m b) -> m b
```

You will sometimes see "us" talk about the use of the bind
do-notation `<-` or `(>>=)` as _binding over_. When we do, we just mean that
we lifted a monadic function and we will eventually `join` or smush
the structure back down when we ar edone monkeying around in the Monad.
_Don't panic_ if we are a little casual about describing the use of `<-` as having
bound over/out some `a` out of `m a`.



