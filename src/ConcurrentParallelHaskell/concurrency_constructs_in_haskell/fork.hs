import Control.Concurrent
import Control.Monad
import System.IO

-- build: ghc -O2 -threaded ./fork.hs
-- run : ./fork
--
main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 100000 (putChar 'A'))
  replicateM_ 100000 (putChar 'B')
