import Data.IORef
import System.Environment
import System.Directory
import System.FilePath
import Data.List hiding (find)
import Control.Exception
import Control.Concurrent -- TVar
import Control.Concurrent.Async
import Control.Concurrent.STM

-- Non-blocking semaphore aka [[NBSem]]
-- [[TVar]] is applied to the quantity semaphore abstraction
--
newtype NBSem = NBSem (TVar Int)

newNBSem :: Int -> STM NBSem
newNBSem i = do
  m <- newTVar i
  return (NBSem m)

tryAcquireNBSem :: NBSem -> STM Int
tryAcquireNBSem (NBSem m) = do
  modifyTVar m $ \i ->
    if i == 0 then i else let z = i - 1 in z
  readTVar m

releaseNBSem :: NBSem -> STM ()
releaseNBSem (NBSem m) =
  modifyTVar m $ \i ->
    let z = i + 1 in z

subfind :: NBSem -> String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        -> [Async (Maybe FilePath)] -> IO (Maybe FilePath)
subfind sem s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
      then inner asyncs
      else do
        q <- atomically $ tryAcquireNBSem sem
        if ((>) q 0) then do
             let dofind = find sem s p `finally` (atomically $ releaseNBSem sem)
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


-- To build: ghc -O2 -threaded -eventlog -rtsopts ./findseq_to_par5.hs
-- To run: ./findseq_to_par5 <n> <file name> <starting directory> +RTS -s -l -N4 
main :: IO ()
main = do
  [n, s, d] <- getArgs
  sem <- atomically $ newNBSem (read n)
  find sem s d >>= print

