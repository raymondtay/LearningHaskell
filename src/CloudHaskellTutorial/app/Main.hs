
module Main where

import System.Environment
import Control.Concurrent (threadDelay)

import Chat -- see the exported functions
import Spawn -- see the exported functions


main :: IO ()
main = do
  [h, p] <- getArgs
  -- launchChat h p -- <1>
  launchSpawnExample h p -- <2>
  
