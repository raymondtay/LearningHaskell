
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -- path to directory entry
        -> Permissions    -- permissions
        -> Maybe Integer  -- file size
        -> ClockTime      -- last modified
        -> Bool
-- The return type pf Predicate is Bool and not IO Bool => pure function w/o I/O.

getFileSize :: FilePath -> IO (Maybe Integer)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind predicate path = getRecursiveContents path >>= filterM check
    where check name = do
        perms    <- getPermissions name
        size     <- getFileSize name
        modified <- getModificationTime name
        return (predicate name perms size modified)

-- `filterM` behaves like the normal filter function, but in this case
-- it evaluates the predicate in the IO monad, allowing the predicate
-- to perform I/O. 

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h    <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\_ -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

