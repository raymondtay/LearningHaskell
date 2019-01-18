{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (catch, finally)
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isControl)
import Data.List (nub)
import Network.URI
import Prelude hiding (catch)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as S

import Network.HTTP

type URL = B.ByteString

data Task = Check URL | Done

main :: IO ()
main = do
  (files, k) <- parseArgs
  let n = length files
  -- count of broken links
  badCount <- newTVarIO (0 :: Int)
  -- for reporting broken links
  badLinks <- newTChanIO
  -- for sending jobs to workers
  jobs <- newTChanIO
  -- the number of workers currently running
  workers <- newTVarIO k
  -- one thread reports bad links to stdout
  forkIO $ writeBadLinks badLinks
  -- start workers threads
  forkTimes k workers (worker badLinks jobs badCount)
  -- read links from files, and enqueues them as jobs
  stats <- execJob (mapM_ checkURLs files) (JobState S.empty 0 jobs)
  -- enqueue "please finish" messages
  atomically $ replicateM_ k (writeTChan jobs Main.Done)

  waitFor workers
  broken <- atomically $ readTVar badCount
  printf fmt broken (linksFound stats) (S.size (linksSeen stats)) n
    where
      fmt = "Found %d broken links. " ++ 
            "Checked %d links (%d unique) in %d files.\n"


modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

-- starts a number of identical worker threads and decreases the "alive" count
-- each time a thread exits. A "finally" combinator to ensure that the count is
-- always decremented, no matter how the thread terminates.
forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k alive act =
  replicateM_ k . forkIO $ act `finally` (atomically $ modifyTVar_ alive (subtract 1))

writeBadLinks :: TChan String -> IO ()
writeBadLinks c = forever $ atomically (readTChan c) >>= putStrLn >> hFlush stdout

waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)


