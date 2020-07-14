

module Concurrent (
  threadCommunication
) where

import Control.Concurrent
{-
 - The runtime component of GHC does not specify an order in which it executes
 - threads. Here's an example:
 -   *Concurrent Control.Concurrent> forkIO (putChar 'X')
 -   ThrXeadId 172
 -   *Concurrent Control.Concurrent> forkIO (putChar 'X')
 -   ThrXeadId 176
 -   *Concurrent Control.Concurrent> forkIO (putChar 'X')
 -   TXhreadId 180
 -   *Concurrent Control.Concurrent> forkIO (putChar 'X')
 -   ThrXeadId 184
 -   *Concurrent Control.Concurrent> forkIO (putChar 'X')
 -   ThrXeadId 188
 - The point of this example is to illustrate the lack of predictability in which the thread executes in the repl
 - and you can see this from the fact that the "putChar" intertwines with the publishing of the thread id.
 -}

threadCommunication :: IO ()
threadCommunication = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received " ++ show v)
  putStrLn "sending"
  putMVar m "Wake up!"


