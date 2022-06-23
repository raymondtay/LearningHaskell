--
-- Source of inspiration https://hackage.haskell.org/package/dlist-1.0/docs/src/Data.DList.Internal.html#UnsafeDList
--
-- ❯ ghc --run -prof -fprof-auto -rtsopts -O2 ./DList.hs
--
-- when making flags consistent: warning:
--     -O conflicts with --interactive; -O ignored.
-- macro 'doc' overwrites builtin command.  Use ':def!' to overwrite.
-- benchmarking concat list
-- time                 911.8 ms   (840.9 ms .. 967.5 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 908.4 ms   (895.7 ms .. 922.1 ms)
-- std dev              16.52 ms   (4.884 ms .. 21.93 ms)
-- variance introduced by outliers: 19% (moderately inflated)
--
-- benchmarking concat dlist
-- time                 936.6 ms   (831.2 ms .. 1.025 s)
--                      0.999 R²   (0.995 R² .. 1.000 R²)
-- mean                 930.1 ms   (905.9 ms .. 949.9 ms)
-- std dev              26.28 ms   (10.09 ms .. 35.59 ms)
-- variance introduced by outliers: 19% (moderately inflated)

module Main where

import           Criterion.Main


newtype DList a = DL { unDL :: [a] -> [a] }

{-# INLINE empty #-}
empty :: DList a
empty = DL id

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton = DL . (:)

{-# INLINE toList #-}
toList :: DList a -> [a]
toList = ($ []) . unDL

{-# INLINE cons #-}
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

{-# INLINE snoc #-}
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x:)

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n - 1) ([n] ++ xs)


constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 1234567,
    bench "concat dlist" $ whnf constructDList 1234567
  ]

