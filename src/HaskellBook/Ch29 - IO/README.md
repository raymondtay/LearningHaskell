# The reason we need this `IO` type

`IO` primarily exists to give us a way to order operations and to disable some
of the sharing that we talked so much about in the chapter on non-strictness.

GHC is ordinarily free to do a lot of reordering of operations, delaying of
evaluation, sharing of named values, duplicating code via inlining and other
optimizations in order to increase performance. The main thing the IO type does
is turn off most of those abilities.

The reason we have `Monad` is because it was a means of abstracting away the
nested lambda noise that underlies `IO` i.e. ```haskell
main = do
  putStr "a"
  putStr "b"
```

# IO's Functor, Applicative and Monad

Another mistake people make is in implying that IO is a Monad, rather than
accounting for the fact that, like all Monads, IO is a datatype that has a
Monad instance - as well as Functor and Applicative instances.

I'm guessing that is also probably why developers mistook that `IO a` is a
_means_ of obtaining `a` by realizing its context i.e. `IO` because of the
time, whenever we see `Just 3 :: Maybe Int` we instinctively take it, for
granted, that its a _value_.

Another interesting example
```haskell

-- remember that randomIO :: Random a => IO a
-- so that we can (randomIO :: IO Int) constraining the type 'a'

(+) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)

-- because the fixity of (+) is also left but higher (i.e. 6) as compared to
-- (<$>) and (<*>) which are both 4 so the binding means we should read it as 
-- ((+) <$> (randomIO :: IO Int)) <*> (randomIO :: IO Int)
-- ((+) <$> (randomIO :: IO Int)) :: IO (Int -> Int) <=== Applicative
-- and when you combine both LHS and RHS its an applicative application.


```


# Monad and IO

for IO, `pure` or `return` can be read as an effect-free embedding of a value
in a recipe-creating environment.

```haskell

import Control.Monad (join)

let embedInIO = return :: a -> IO a
embedInIO 1 -- this prints 1 onto the console
embedInIO (print "hello world!") -- this prints nothing

-- the reason is because the latter expression is of type `IO (IO ())`
-- and to run this we need to use `join`

join $ embedInIO (print "Hello World!") -- prints "Hello World!" to the console
without the quotes

join $ embedInIO (embedInIO 1)

```

# Monadic associativity

The following passage is lifted from page 1175 of the book
```

Haskellers will often get confused when they are told Monad's bind is
associative because they will think of IO as a counter-example. The mistake
being made here is mistaking the construction of IO actions for the execution
of IO actions. As far as Haskell is concerned, we only constuct IO actions to
be executed when we call _main_. Semantically, IO actions are not somethjing we
do, but something we talk about. 

Binding over an IO action does not execute it, it produces a new IO action i
terms of the old one.

You can reconcile yourself with this framing by remembering how IO actions are
like recipes, an analogy created by Brent Yorgey that we are fond of.

```


