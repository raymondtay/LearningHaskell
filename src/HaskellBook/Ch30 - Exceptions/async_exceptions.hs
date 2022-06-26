module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception
import           System.IO

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "test.dat" WriteMode
  threadDelay 15
  hPutStr h (replicate 100000000 '0' ++ "abc") -- dependent on whether you have fast IO
  hClose h

data PleaseDie = PleaseDie deriving Show

instance Exception PleaseDie

main :: IO ()
main = do
  threadId <- forkIO openAndWrite
  threadDelay 1
  throwTo threadId PleaseDie


