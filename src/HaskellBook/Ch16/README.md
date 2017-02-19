# Functors

## Functor is function application

In fact, `fmap` is a specific sort of function application. Let's look at the
types:
`fmap :: Functor f => (a -> b) -> f a -> f b`
There's also an infix operator for `fmap`. If you are using an older version of
GHC, you may need to import `Data.Functor` in order to use it in the REPL. Of
course, it has the same type as the prefix `fmap`:
`(<$>) :: Functor f => (a -> b) -> f a -> f b`

