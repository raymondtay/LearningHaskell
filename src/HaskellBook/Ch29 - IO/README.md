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

