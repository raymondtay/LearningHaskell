{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

import DistribUtils
import Control.Monad
import Data.Binary
import Data.Typeable
import GHC.Generics -- Don't use [[Data.Generics]]
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Text.Printf -- for [[printf]]

-- notice that there is NO Pong message; read further
data Message = Ping (SendPort ProcessId) deriving (Typeable, Generic)

instance Binary Message

-- Simple ping server process. This server will wait for a Ping message and
-- then respond with a return message that carries its process id
pingServer :: Process ()
pingServer = do
  Ping chan <- expect
  say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  sendChan chan mypid

remotable ['pingServer]


master :: [NodeId] -> Process ()
master peers = do
  ps <- forM peers $ \nid -> do
    say $ printf "spawning on %s" (show nid)
    spawn nid $(mkStaticClosure 'pingServer)

  ports <- forM ps $ \pid -> do -- attempt to send Ping messages to all the peers
    say $ printf "pinging %s" (show pid)
    (sendport, recvport) <- newChan
    send pid (Ping sendport)
    return recvport

  oneport <- mergePortsBiased ports
  waitForPongs oneport ps


  say "All messages successfully received"
  terminate


waitForPongs :: ReceivePort ProcessId -> [ProcessId] -> Process ()
waitForPongs _ [] = return ()
waitForPongs port ps = do
  pid <- receiveChan port
  waitForPongs port (filter (/= pid) ps)
{-

To build: ghc -O2 ./ping-typed-merge-chan.hs
To run: ./ping-typed-merge-chan [slave 127.0.0.1|0.0.0.0] 44445 &
        ./ping-typed-merge-chan [slave 127.0.0.1|0.0.0.0] 44446 &
        ./ping-typed-merge-chan

Fri Jan 25 02:58:19 UTC 2019 pid://localhost:44444:0:8: spawning on nid://0.0.0.0:44445:0
Fri Jan 25 02:58:19 UTC 2019 pid://localhost:44444:0:8: spawning on nid://0.0.0.0:44446:0
Fri Jan 25 02:58:19 UTC 2019 pid://localhost:44444:0:8: pinging pid://0.0.0.0:44445:0:9
Fri Jan 25 02:58:19 UTC 2019 pid://localhost:44444:0:8: pinging pid://0.0.0.0:44446:0:9
Fri Jan 25 02:58:19 UTC 2019 pid://0.0.0.0:44445:0:9: ping received from SendPort {sendPortId = cid://localhost:44444:0:8:0}
Fri Jan 25 02:58:19 UTC 2019 pid://0.0.0.0:44446:0:9: ping received from SendPort {sendPortId = cid://localhost:44444:0:8:1}
Fri Jan 25 02:58:19 UTC 2019 pid://localhost:44444:0:8: All messages successfully received
ping-typed-chan: ProcessTerminationException

On my mac, this is necessary as there's some discrepancy with running as per the book 
suggests and explicit IPv4 address was given. IPv6 address is not working on my mac - its likely
something to do with the package here.

-}

main :: IO ()
main = distribMain master Main.__remoteTable





