{-
 - ❯ ghc --run -prof -fprof-auto -rtsopts -O2 ./mutable_vectors_bench.hs
 -
 - when making flags consistent: warning:
 -     -O conflicts with --interactive; -O ignored.
 - macro 'doc' overwrites builtin command.  Use ':def!' to overwrite.
 - benchmarking mutable IO vector
 - time                 5.302 ms   (5.264 ms .. 5.349 ms)
 -                      0.999 R²   (0.998 R² .. 1.000 R²)
 - mean                 5.305 ms   (5.282 ms .. 5.374 ms)
 - std dev              112.8 μs   (36.98 μs .. 228.2 μs)
 -
 - benchmarking mutable ST vector
 - time                 5.341 ms   (5.322 ms .. 5.361 ms)
 -                      1.000 R²   (1.000 R² .. 1.000 R²)
 - mean                 5.336 ms   (5.322 ms .. 5.348 ms)
 - std dev              44.77 μs   (36.94 μs .. 55.85 μs)
-}

module Main where

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Criterion.Main
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as MV

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
    where go 0 v = return v
          go n v = (MV.write v n 0) >> go (n-1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec
    where go 0 v = V.freeze v
          go n v = (MV.write v n 0) >> go (n-1) v

main :: IO ()
main = defaultMain
  [ bench "mutable IO vector"
  $ whnfIO (mutableUpdateIO 9998),
  bench "mutable ST vector" $ whnf mutableUpdateST 9998]


