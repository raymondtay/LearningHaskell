import MyAsync -- symlink to external location
import Text.Printf
import System.IO
import Data.ByteString as B
import Data.Either
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.Async
import GetURL -- symlink to external location
import Control.Monad
import Data.Time

-- original author: simon marlow
timeit :: IO a -> IO (a,Double)
timeit io = do
     t0 <- getCurrentTime
     a <- io
     t1 <- getCurrentTime
     return (a, realToFrac (t1 `diffUTCTime` t0))


wait :: MyAsync.Async a -> STM (Either SomeException a)
wait (MyAsync.Async _ var) = readTMVar var

timeDownload :: String -> IO ()
timeDownload url = do
  (page, time) <- timeit $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time


sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.org/wiki/Spade",
         "http://www.wikipedia.org/wiki/Shovel"
        ]

-- main :: IO ()
-- main = do
--   let
--     download url = do
--       r <- getURL url
--       return (url, r)
--   as <- mapM (async . download) sites
--   (url, r) <- waitAny as
--   printf "%s was first (%d bytes)\n" url (B.length r)
--   atomically $ mapM_ wait as

main :: IO ()
main = do
      xs <- Prelude.foldr concExec (return []) (Prelude.map getURL sites)
      print (Prelude.map B.length xs)
    where
      concExec ioa ioas = do
        (a, as) <- concurrently ioa ioas
        return (a:as)


