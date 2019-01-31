module Chat (
  replyBack,
  logMessage,
  localBind
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forever)
import qualified Control.Distributed.Process as P
import Control.Distributed.Process.Node
import Network.Socket (ServiceName, HostName)
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)

replyBack :: (P.ProcessId, String) -> P.Process ()
replyBack (sender, msg) = P.send sender msg

logMessage :: String -> P.Process ()
logMessage msg = P.say $ "handling " ++ msg

-- Regardless of [[host]] provided, we will map to the IPv4 address
-- but the port to be bound will be respected.
localBind :: HostName ->
             ServiceName ->
             IO (Either IOException Transport)
localBind host service = createTransport host service (\_ -> ("0.0.0.0", service)) defaultTCPParameters

