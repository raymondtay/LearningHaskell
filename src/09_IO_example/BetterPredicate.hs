
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

{-
    The bracket function takes three actions as arguments. The first action acquires a 
    resource. The second releases the source. The third runs in between, while the resource
    is acquired; let's call this the "use" action. If the "acquire" action succeeds, the 
    "release" action is always called. This guarantees that the resource will always be released.
    The "use" and "release" actions are each passed the resource acquired by the "acquire" action.
-}
getFileSize path = handle (\_ -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

type InfoP a = FilePath
        -> Permissions
        -> Maybe Integer
        -> ClockTime
        -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

