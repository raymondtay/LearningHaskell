{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

import DistribUtils
import Control.Monad
import Data.Binary
import Data.Typeable
import GHC.Generics -- Don't use [[Data.Generics]]
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Text.Printf -- for [[printf]]

data Message = Ping ProcessId | Pong ProcessId deriving (Typeable, Generic)

instance Binary Message

-- Simple ping server process. This server will wait for a Ping message and
-- then respond with a Pong message.
pingServer :: Process ()
pingServer = do
  Ping from <- expect
  say $ printf "ping received from %s" (show from)
  mypid <- getSelfPid
  send from (Pong mypid)

remotable ['pingServer]

-- Its very easy to miss it but `pingServer` is ∈ Process Monad and only
-- code running in the Process monad can communicate with other processes and
-- spawn new processes.
-- 
-- expect :: Serializable a => Process a
--
-- interesting type signature because of a few reasons:
--  (i) Process expects a message of type a and it knows that this message must
--      serializable (that's important because it means that the data can be
--      passed in and out of the network with no translation problems when it
--      reaches either end of the communication channel)
--
--  (ii) Process abstracts away the location of the processing of this message
--       per se. That might look like a minute detail but it reduces the
--       cognitive overload the developer has to deal with.
--
--  (iii) If there are no messages of the right type, [[expect]] will block
--        until one arrives. Therefore, it should be used with care: the other
--        messages in the queue are ignored while [[expect]] is waiting for the
--        right kind of message to arrive, which could lead to a deadlock.
--      

master :: [NodeId] -> Process ()
master peers = do
  liftIO $ putStrLn $ show peers -- an additional detail to show the kinds of peers in plain sight.
  -- you should see a printout like the following:
  -- [nid://127.0.0.1:44445:0,nid://127.0.0.1:44446:0]
  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)
  mypid <- getSelfPid

  forM_ ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    send pid (Ping mypid)

  waitForPongs ps

  say "All pongs successfully received"
  terminate

waitForPongs :: [ProcessId] -> Process ()
waitForPongs [] = return ()
waitForPongs ps = do
  m <- expect
  case m of
      Pong p -> waitForPongs (filter (/= p) ps)
      _ -> say "MASTER received ping" >> terminate

{-

To build: ghc -O2 ./ping-multi.hs
To run: ./ping-multi [slave 127.0.0.1|0.0.0.0] 44445 &
        ./ping-multi [slave 127.0.0.1|0.0.0.0] 44446 &
        ./ping-multi

Thu Jan 24 03:42:45 UTC 2019 pid://localhost:44444:0:8: spawning on nid://127.0.0.1:44445:0
Thu Jan 24 03:42:45 UTC 2019 pid://localhost:44444:0:8: spawning on nid://127.0.0.1:44446:0
Thu Jan 24 03:42:45 UTC 2019 pid://localhost:44444:0:8: pinging pid://127.0.0.1:44445:0:9
Thu Jan 24 03:42:45 UTC 2019 pid://localhost:44444:0:8: pinging pid://127.0.0.1:44446:0:9
Thu Jan 24 03:42:45 UTC 2019 pid://127.0.0.1:44445:0:9: ping received from pid://localhost:44444:0:8
Thu Jan 24 03:42:45 UTC 2019 pid://127.0.0.1:44446:0:9: ping received from pid://localhost:44444:0:8
Thu Jan 24 03:42:45 UTC 2019 pid://localhost:44444:0:8: All pongs successfully received
ping-multi: ProcessTerminationException

On my mac, this is necessary as there's some discrepancy with running as per the book 
suggests and explicit IPv4 address was given. IPv6 address is not working on my mac - its likely
something to do with the package here.

-}

main :: IO ()
main = distribMain master Main.__remoteTable

