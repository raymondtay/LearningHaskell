import System.Environment
import Text.Printf

main = do
  [a] <- map read `fmap` getArgs
  printf "%f\n" (mean [1..a])

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where (n, s) = foldl k (0, 0) xs
        k (n, s) x = (n+1, s+x)

-- problem is that foldl' is too lazy

--
-- In ghc 8.4.3, its not quite the same as in the RWH book and who can blame
-- them coz its been 7 years!
-- Build : ghc --make -O2 -rtsopts -prof -fprof-auto -fprof-cafs A.hs
-- Run   : time ./A 1e5 +RTS -hc -hd -hy -s -- 100,000
-- Run   : time ./A 1e6 +RTS -hc -hd -hy -s -- 1,000,000
-- Run   : time ./A 1e7 +RTS -hc -hd -hy -s -- 10,000,000
