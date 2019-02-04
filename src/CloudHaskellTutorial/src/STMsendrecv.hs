module STMSendRecv (
  launchSTM
  ) where

import Chat
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import Control.Monad.IO.Class
import qualified Control.Distributed.Process as P
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Socket (ServiceName, HostName)
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- Simple function that performs a STM action
-- see [[Main.hs]] on how its being used.
an_example :: STM (Int, Int)
an_example = do
    a  <- newEmptyTMVar
    b  <- newEmptyTMVar
    _  <- putTMVar a 1
    _  <- putTMVar b 3
    _a <- takeTMVar a
    _b <- takeTMVar b
    return (_a, _b)

toActivate :: (Int, Int) -> P.Process ()
toActivate (1, _) = P.say $ "Activated!"
toActivate (_, _) = P.say $ "Nah, you didn't give me the right combination!"

-- Launch a local process
launchSTM :: HostName -> ServiceName -> IO ()
launchSTM h p = do
  Right t <- localBind h p -- e.g. "127.0.0.1" "10501" 
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    echoPid <- P.spawnLocal $ forever $ do
      P.receiveWait [P.matchSTM an_example toActivate]
    P.say "Activating a STM example"
    P.send echoPid ()
    liftIO $ threadDelay 200000

