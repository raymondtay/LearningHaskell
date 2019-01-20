import Data.IORef
import System.Environment
import System.Directory
import System.FilePath
import Data.List hiding (find)
import Control.Exception
import Control.Concurrent -- MVar
import Control.Concurrent.Async

-- Non-blocking semaphore aka [[NBSem]] and this flavour is different from
-- [[findseq_to_par2]] in the respect that we do not use STM and use IORef
-- instead.
-- The point of these exercises is probably to point out that Haskell's toolkit
-- allows developers to experiment with control structures for threads like
-- MVars, TVars to get started with and how the developer can progress to lift
-- and leverage other facilities like IORef (recall that its actually mutable
-- references in the IO monad, which is where most of this code runs in)
--
--
newtype NBSem = NBSem (IORef Int)

newNBSem :: Int -> IO NBSem
newNBSem i = do
  m <- newIORef i
  return (NBSem m)

tryWaitNBSem :: NBSem -> IO Bool
tryWaitNBSem (NBSem m) =
  atomicModifyIORef m $ \i ->
    if i == 0 then (i, False) else let !z = i - 1 in (z, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  atomicModifyIORef m $ \i ->
    let !z = i + 1 in (z, ())

subfind :: NBSem -> String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        -> [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subfind sem s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
      then inner asyncs
      else do
        q <- tryWaitNBSem sem
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


-- To build: ghc -O2 -threaded -eventlog -rtsopts ./findseq_to_par3.hs
-- To run: ./findseq_to_par3 <file name> <starting directory> +RTS -s -l -N4 
main :: IO ()
main = do
  [s, d] <- getArgs
  n <- getNumCapabilities
  sem <- newNBSem (if n == 1 then 0 else n * 4)
  find sem s d >>= print

