module IORefTrans where
-- ❯ ghc --run -prof -fprof-auto -rtsopts -O2 ./IORefTrans.hs
--
-- when making flags consistent: warning:
--     -O conflicts with --interactive; -O ignored.
-- macro 'doc' overwrites builtin command.  Use ':def!' to overwrite.
-- benchmarking gimmeShelter
-- time                 212.9 ns   (211.9 ns .. 214.1 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 212.3 ns   (211.6 ns .. 213.6 ns)
-- std dev              2.979 ns   (1.979 ns .. 4.343 ns)
-- variance introduced by outliers: 15% (moderately inflated)
--
-- benchmarking gimmeShelter
-- time                 199.8 ns   (199.3 ns .. 200.6 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 200.0 ns   (199.5 ns .. 200.6 ns)
-- std dev              1.761 ns   (1.318 ns .. 2.491 ns)

import           Control.Monad  (replicateM)
import           Criterion.Main
import           System.Random  (randomRIO)

gimmeShelter :: Bool -> IO [Int]
gimmeShelter True  = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]

-- The trick here is to realize that while executing `IO[Int]`
-- can and does produce different literal values when the argument
-- is True, it's still producing the same result (i.e. a list of
-- random numbers) for the same input. Referential transparency is
-- preserved because we are still returning the same IO action,
-- or "recipe", for the same argument, the same means of obtaining
-- a list of Int. Every True input to this function will return a list of random Ints:

main :: IO ()
main = defaultMain
  [ bench "gimmeShelter" $ whnf gimmeShelter True,
    bench "gimmeShelter" $ whnf gimmeShelter False]

