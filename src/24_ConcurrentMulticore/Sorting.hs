module Sorting where

import Control.Parallel (par, pseq)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)

parSort :: (Ord a) => [a] -> [a]
parSort (x:xs) = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
  where lesser = parSort [y | y <- xs, y < x]
        greater = partSort [y | y <- xs, y >= x]
partSort _ = []

force :: [a] -> ()
force xs = go xs `pseq` ()
  where go [] = 1
        go (_:xs) = go xs


testFunction = parSort

randomInts :: Int -> StdGen -> [Int]
randomInts k g = let result = take k (randoms g) in force result `seq` result

main = do
  args <- getArgs
  let count | null args = 500000
            | otherwise = read (head args)
  input <- randomInts count `fmap` getStdGen
  putStrLn $ "We have " ++ show (length input) ++ " elements to sort."
  start <- getCurrentTime
  let sorted = testFunction input
  putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."

