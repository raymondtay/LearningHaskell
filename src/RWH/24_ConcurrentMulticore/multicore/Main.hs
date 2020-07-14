
module Main where

import Concurrent
import Control.Concurrent


main :: IO ()
main = forkIO (write 'a') >> write 'c'
  where write c = putChar c >> write c

