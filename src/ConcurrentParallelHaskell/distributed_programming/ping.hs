{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

import DistribUtils

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

master :: Process ()
master = do
  node <- getSelfNode

  say $ printf "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid
  say $ printf "sending ping to %s" (show pid)
  send pid (Ping mypid)

  Pong _ <- expect
  say "pong. "

  terminate

{-

To build: ghc -O2 ./ping.hs
To run: ./ping 

Thu Jan 24 02:02:15 UTC 2019 pid://localhost:44444:0:8: spawning on nid://localhost:44444:0
Thu Jan 24 02:02:15 UTC 2019 pid://localhost:44444:0:8: sending ping to pid://localhost:44444:0:9
Thu Jan 24 02:02:15 UTC 2019 pid://localhost:44444:0:9: ping received from pid://localhost:44444:0:8
Thu Jan 24 02:02:15 UTC 2019 pid://localhost:44444:0:8: pong.
ping: ProcessTerminationException


Some notes are in order: 

1/ To create a process, we call `spawn`, passing a `NodeId` and a `Closure (Process ())`. The former we got
   from `getSelfNode` (there are other ways, which we will encounter shortly), and the latter was generated
   by a call to the Template Haskell function `mkStaticClosure`.

2/ Processes run in the `Process` monad, which is a layer over the IO monad.

3/ Messages can be sent to a process using `send` and received by the calling `expect`. Messagess
   are ordinarily Haskell data; the only requirement is that the type of the message is an instance
  of the `Binary` and `Typeable` classes.

There's a certain amount of boilerplate associated with distributed programming: deriving `Binary` instances
declaring remotable functions with `remotable`, starting up the framework with `distribMain` and so on.

-}

main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable

