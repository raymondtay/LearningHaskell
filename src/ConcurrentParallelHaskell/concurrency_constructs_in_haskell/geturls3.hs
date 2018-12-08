import Text.Printf
import System.IO
import Data.ByteString as B
import Data.Either
import Control.Exception
import Control.Concurrent
import GetURL
import Control.Monad
import Data.Time

-- original author: simon marlow
timeit :: IO a -> IO (a,Double)
timeit io = do
     t0 <- getCurrentTime
     a <- io
     t1 <- getCurrentTime
     return (a, realToFrac (t1 `diffUTCTime` t0))

-- The rationale for this new definition is that we need the thread-id of the
-- thread that is running the Async, so we must store that type alongside the
-- MVar related to the Async
data Async a = Async ThreadId (MVar (Either SomeException a))

-- The ThreadKilled exception is provided by the Control.Exception library and
-- is typically used for cancelling threads in this way.
--
cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled

async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  t <- forkIO (do r <- try action; putMVar m r)
  return (Async t m)


timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ v) = readMVar v

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.org/wiki/Spade",
         "http://www.wikipedia.org/wiki/Shovel"
        ]

main = do
  as <- mapM (async . timeDownload) sites

  forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ do
      c <- getChar
      when (c == 'q') $ mapM_ cancel as
  rs <- mapM waitCatch as
  printf "%d/%d succeeded\n" (Prelude.length (rights rs)) (Prelude.length rs)

