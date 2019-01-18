import Data.IORef
import System.Environment
import System.Directory
import System.FilePath
import Data.List hiding (find)
import Control.Exception
import Control.Concurrent -- MVar
import Control.Concurrent.Async

-- Non-blocking semaphore aka [[NBSem]]
--
newtype NBSem = NBSem (MVar Int)

newNBSem :: Int -> IO NBSem
newNBSem i = do
  m <- newMVar i
  return (NBSem m)

tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) =
  modifyMVar m $ \i ->
    if i == 0 then return (i, False) else let z = i - 1 in return (z, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  modifyMVar m $ \i ->
    let z = i + 1 in return (z, ())

subfind :: NBSem -> String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        -> [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subfind sem s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
      then inner asyncs
      else do
        q <- tryAcquireNBSem sem
        if q then do
             let dofind = find sem s p `finally` releaseNBSem sem
             withAsync dofind $ \a -> inner (a:asyncs)
             else do
               r <- find sem s p
               case r of
                   Nothing -> inner asyncs
                   Just _ -> return r

-- See [[findseq_to_par.hs]] for more details on this function.
-- the only minor addition to this is the addition of the non-blocking
-- semaphore which is carried all over this function, it does nothing at all.
--
find :: NBSem -> String -> FilePath -> IO (Maybe FilePath)
find sem s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs -- want only real files, not present or parent directories
  if any (== s) fs' then return (Just (d </> s)) else loop fs'
    where
      loop [] = return Nothing
      loop (f:fs) = do
        let d' = d </> f
        isdir <- doesDirectoryExist d'
        if isdir then
                 do r <- find sem s d'
                    case r of
                        Just _ -> return r
                        Nothing -> loop fs
                  else loop fs


-- To build: ghc -O2 -threaded -eventlog -rtsopts ./findseq_to_par2.hs
-- To run: ./findseq_to_par2 <n> <file name> <starting directory> +RTS -s -l -N4 
main :: IO ()
main = do
  [n, s, d] <- getArgs
  sem <- newNBSem (read n)
  find sem s d >>= print

-- notes:
--
-- How can we improve the NBSem implementation to behave better when there is
-- contention ? One solution would be to use STM because STM transactions do
-- not block, they just re-execute repeatedly.
-- 
-- Accortding to the claims on page 235, STM does work here but it is suggested
-- by the author to attempt a different way to solve the problem, one that has
-- less overhead than STM. The idea is to use an ordinary [[IORef]] to store
-- the semaphore value and operate on it using [[atomicModifyIORef]] 
--
--
