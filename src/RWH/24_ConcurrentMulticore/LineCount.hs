module Main where

import Control.Monad (forM_)
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Environment (getArgs)
import LineChunks (chunkedReadWith)
import MapReduce(mapReduce)

-- This function is interesting because we 
lineCount :: [LB.ByteString] -> Int64
lineCount = mapReduce rpar (LB.count '\n') rpar sum

-- make : ghc -O2 --make -threaded ./LineCount.hs
-- 4-core, 2-core and then single-core processing.
--
-- run  : ./LineCount +RTS -N4 -RTS ~/Downloads/log_data/logXXX.csv
-- run  : ./LineCount +RTS -N2 -RTS ~/Downloads/log_data/logXXX.csv
-- run  : ./LineCount +RTS -N1 -RTS ~/Downloads/log_data/logXXX.csv
--
main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \path -> do
    numLines <- chunkedReadWith lineCount path
    putStrLn $ path ++ ": " ++ show numLines


