
import System.Environment
import System.Directory
import System.FilePath
import Data.List hiding (find)
import Control.Exception

import Control.Monad.Par.IO
import Control.Monad.Par.Class
import Control.Monad.IO.Class


find :: String -> FilePath -> ParIO (Maybe FilePath)
find s d = do
  fs <- liftIO $ getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else do
       let ps = map (d </>) fs'
       foldr (subfind s) dowait ps []
 where
   dowait vs = loop (reverse vs)

   loop [] = return Nothing
   loop (v:vs) = do
      r <- get v
      case r of
        Nothing -> loop vs
        Just a  -> return (Just a)


subfind :: String -> FilePath
        -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath))
        -> [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)
subfind s p inner ivars = do
  isdir <- liftIO $ doesDirectoryExist p
  if not isdir
     then inner ivars
     else do v <- new                   -- <1>
             fork (find s p >>= put v)  -- <2>
             inner (v : ivars)          -- <3>

-- To build: ghc -O2 -threaded -eventlog -rtsopts ./findseq_to_par4.hs
-- To run: ./findseq_to_par4 <file name> <starting directory> +RTS -s -l -N4 
main :: IO ()
main = do
  [s, d] <- getArgs
  runParIO (find s d) >>= print

-- Notes: Page 238 of the book reads
--
-- This verion beats our carefully coded NBSem implementation, achieving a
-- speedup of 2.92 on 4 cores. Why is that? Well, one reason is that we didn't
-- have to consult some shared state and choose whether to fork or continue our
-- operation in the current thread, beacuse fork is very cheap in Par and ParIO
-- (note the low memory overhead in the results above). Another reason is that
-- the Par monad has a carefully tuned work-stealing scheduler implementation
-- that is designed to achieve good parallel speedup.
--

