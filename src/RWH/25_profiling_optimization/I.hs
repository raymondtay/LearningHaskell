
-- | in ghc 8.4.3, i need the "uvector" package
-- but for some reason i'm unable to install it locally. 
import System.Environment
import Text.Printf
import Data.Array.Vector

main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean (enumFromToFracU 1 d))


data Pair = Pair !Int !Double

mean :: UArr Double -> Double
mean xs = s / fromIntegral n
  where Pair n s = foldU k (Pair 0 0) xs
        k (Pair n s) x = Pair (n+1) (s+x)

