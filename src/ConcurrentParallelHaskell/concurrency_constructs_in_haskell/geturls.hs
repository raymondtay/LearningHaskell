import Control.Concurrent
import Data.ByteString as B
import GetURL
import System.TimeIt
import System.IO
import Control.Monad        (forM)
import Control.Exception
import Text.Printf
--
-- 2 MVars to hold the result of the concurrent call.
--
version1 = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar

  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Shovel"
    putMVar m1 r

  forkIO $ do
    r <- getURL "http://www.wikipedia.org/wiki/Japan"
    putMVar m2 r

  r1 <- takeMVar m1
  r2 <- takeMVar m2
  print (B.length r1, B.length r2)

-- Building this abstraction allows us to describe a asynchronous effect
-- on a thread.
data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- action; putMVar var r)
  return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

version2 = do
  a1 <- async $ (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async $ (getURL "http://www.wikipedia.org/wiki/Japan")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)

sites :: [String]
sites = [ "http://www.linkedin.com",
          "http://www.yahoo.com",
          "http://www.twitter.com",
          "http://cnn.com",
          "http://bloomberg.com"
        ]
version3 = do
  timeTaken <- forM sites (\site ->
    (do m1 <- newEmptyMVar; (t, a1) <- timeItT $ async $ getURL site; r1 <- wait a1; return t))
  print $ "Total time taken: " ++ show (sum timeTaken)

main = do
  version4
  -- version3
  -- version2
  -- version1
  --

timeDownload :: String -> IO ()
timeDownload url = do
  (time, page) <- timeItT $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

version4 = do
  dontcare <- mapM (async . timeDownload) sites
  mapM_ wait dontcare

-- in ghc 7.4 and earlier, the `Prelude` exports a function call `catch`, which
-- is similar to `Control.Exception.catch` but restricted to `IOExceptions`. If
-- you are using exceptions with GHC 7.4.x or earlier, you should use the
-- following:
--
-- import Control.Exception
-- import Prelude hiding (catch)
--
-- note that this code still works with GHC 7.6.1. and later because it is now
-- a warning, rather than an error, to mention a nonexistent identifier in a
-- hiding clause. It is always better to use throwIO rather throw in the IO
-- monad because throwIO guarantees strict ordering with respect to other IO
-- operations, whereas throw does not.
--
--

simpleDemo =
  bracket (openFile "/Users/raymondtay/.bash_profile" ReadMode)
          (hClose)
          (\file -> do { contents <- System.IO.hGetContents file; System.IO.putStrLn contents })

