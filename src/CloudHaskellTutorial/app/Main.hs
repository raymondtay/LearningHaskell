module Main where

import System.Environment
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node -- for [[newLocalNode]]

import Chat -- see the exported functions
-- import Lib


main :: IO ()
main = do
  [h, p] <- getArgs
  Right t <- localBind h p -- e.g. "127.0.0.1" "10501" 
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    echoPid <- spawnLocal $ forever $ do
      receiveWait [match logMessage, match replyBack]
    say "Send some messages!"
    send echoPid "hello"
    self <- getSelfPid
    send echoPid (self, "hello")
    m <- expectTimeout 1000000
    case m of
        Nothing -> die "nothing came back!"
        Just x -> say $ "got " ++ x ++ " back! "
    liftIO $ threadDelay 2000000

