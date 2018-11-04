import Control.Monad.Par
import Control.Monad.IO.Class

-- The canonical quick sort, remarkably similar to mergesort and easily
-- mistaken for it if i'm not scrutinizing.
--
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = lesser ++ [x] ++ greater
  where lesser = [a | a <- xs, a < x]
        greater = [a | a <- xs, a >= x]

par_quicksort :: (NFData a, Ord a) => [a] -> Par [a]
par_quicksort [] = return []
par_quicksort (x:xs) = do
  p1 <- spawn (par_quicksort lesser)
  p2 <- spawn (par_quicksort greater)
  left <- get p1
  right <- get p2
  return $ left ++ (x:right)
    where
      lesser = [a | a <- xs, a < x]
      greater = [a | a <- xs, a >= x]

-- Build : ghc -O2 -threaded -rtsopts -eventlog ./par_quicksort.hs
-- Run   : ./par_quicksort +RTS -N4 -ls
--
-- The sequential version is noticably (slightly) faster on my 13' macbook pro
-- as compared to the parallel version
--
main :: IO ()
main = do
  let seq_result = quicksort ((reverse [1..10000]) :: [Int])
  putStrLn "Seq version is Done"
  runParIO $ par_quicksort ((reverse [1..10000]) :: [Int])
  putStrLn "Par version is Done"

