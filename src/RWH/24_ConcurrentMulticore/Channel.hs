
import Control.Concurrent
import Control.Concurrent.Chan

-- 1-way communication channel.
--
chanExample = do
  ch <- newChan
  forkIO $ do
    writeChan ch "aaaa" -- writeChan never blocks
    writeChan ch "bbbb"
  readChan ch >>= print -- potentially blocking when no data to read
  readChan ch >>= print


