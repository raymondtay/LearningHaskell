{-# LANGUAGE TemplateHaskell #-}

module Spawn (
  launchSpawnExample,
  sampleTask
  ) where


import Chat (localBind) -- borrowing from [[Chat]]
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Socket (ServiceName, HostName)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Spawn.__remoteTable initRemoteTable

launchSpawnExample :: HostName -> ServiceName -> IO ()
launchSpawnExample h p = do
  Right transport <- localBind h p
  node <- newLocalNode transport myRemoteTable
  runProcess node $ do
    me <- getSelfNode
    _  <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
    pid <- spawn me $ $(mkClosure 'sampleTask) (1:: Int, "using spawn")
    liftIO $ threadDelay $ 2000000


