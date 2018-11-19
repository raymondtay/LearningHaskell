
import Text.Printf
import Data.ByteString as B
import Control.Exception
import Control.Concurrent
import GetURL
import Control.Monad

data Async a = Async (MVar (Either SomeException a))


async :: IO a -> IO (Async a)
async action = do
  v <- newEmptyMVar
  forkIO (do r <- try action; putMVar v r)
  return (Async v)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async v) = readMVar v

wait :: Async  a-> IO a -- the main difference between here and geturls.hs
wait a = do
  r <- waitCatch a
  case r of 
      Left e -> throwIO e
      Right x -> return x

sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.org/wiki/Spade",
         "http://www.wikipedia.org/wiki/Shovel"
        ]

-- `waitAny` represents the idea of collecting the results of each asynchronous
-- action in the given container where each _action_ is executed in the
-- auspices of a CPU thread with error capture built-in.
--
waitAny :: [Async a] -> IO a
waitAny xs = do
  m <- newEmptyMVar
  let forkwait f = forkIO (do r <- try (wait f); putMVar m r)
  mapM_ forkwait xs
  wait (Async m)


-- This example is another classic example of how we can design an abstraction
-- using MVars to solve a problem that is as follows:
-- Suppose we want to wait for one of several different events to occur. For
-- example, when downloading multiple URLs, we want to perform some action as
-- soon as the first one has downloaded.
--
version5 = do
  n <- newEmptyMVar
  let
    download url = do
      r <- getURL url
      putMVar n (url, r)
  mapM_ (forkIO . download) sites

  (url, r) <- takeMVar n
  printf "%s was first (%d bytes)\n" url (B.length r)
  replicateM_ (Prelude.length sites - 1) (takeMVar n)

-- This rendition was developed to counter the deficiencies found in `version5`
-- and the first thing you will notice is that the inter-thread coordination
-- via the MVar is removed.
--
version6 = do
  let
    download url = do
      r <- getURL url
      return (url, r)

  fs <- mapM (async . download) sites
  (url, r) <- waitAny fs
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait fs


main :: IO ()
main = do
  version6
  -- version5

