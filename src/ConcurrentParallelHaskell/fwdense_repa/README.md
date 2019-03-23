# Monads and computeP

Did we really need to thread a monad through the go function? Strictly
speaking, the answer is no, because you can always replace computeS with
(runIdentity . computeP) but this can lead to trouble. To illustrate what can
go wrong, let's compute two arrays with computeP, where the second will depend
on the first. The first is just a vector of Int:

```haskell
  > let arr = fromFunction (Z:.5) (\(Z:i) -> i :: Int)
  > let parr = runIdentity $ computeP arr :: Array U DIM1 Int
```

And the second is a copy of the first, using fromFunction again:
```haskell
  > let arr2 = fromFunction (Z:.5) (\ix -> parr ! ix)
```
Now when we try to compute the second array using computeP, we get an error
message and the key message here is that a call to computeP cannot refer to
another array calculated with computeP, unless the inner computeP has already
been evaluated. Here, we didn't evaluate it; we just bound it with let, using
runIdentity to satisfy the Monad requirement.

The monad requirement in computeP is there to help us avoid this problem,
because computeP ensures that the result is fully evaluated in the monad. In
GHCi, we can use the IO monad:

```haskell
  > let arr = fromFunction (Z:.5) (\(Z:i) -> i :: Int)
  > parr <- computeP arr :: IO (Array U DIM1 Int)
  > let arr2 = fromFunction (Z:.5) (\ix -> parr ! ix)
  > computeP arr2 :: IO (Array U DIM1 Int)
  ...
```

So this is the rule of thumb: if your program makes multiple calls to computeP,
try to ensure that they are performed in the same monad.

The function argument used with foldP must be associative. That is, the
funciton f must satisfy f x (f y z) = f (f x y) z. This is because unlike
foldS, foldP doesn't necessarily fold the funciton over the array elements in
strict left-to-right order; it folds different parts of the array in parallel
and then combines the results from those parts using the folding function.

Note that strictly speaking, althought mathematical addition is associative,
floating-point addition is not, due to rounding errors. However, we tend to
ignore this detail when using foldP because a small amountn of nondeterminism
in the floating point result is normally acceptable.


