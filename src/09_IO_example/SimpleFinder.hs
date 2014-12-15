
import RecursiveContents (getRecursiveContents)
import System.Directory (doesDirectoryExist, getCurrentDirectory)

-- Naive version (that's how anyone learns i'd say, you start from something small and work your way)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind predicate path = do
    names <- getRecursiveContents path
    return (filter predicate names)

-- here's how you might use this function 'simpleFind'
-- *Main Control.Monad> import System.FilePath
-- *Main Control.Monad System.FilePath> takeExtension "foo/bar.c"
-- Loading package filepath-1.3.0.2 ... linking ... done.
-- ".c"
-- *Main Control.Monad System.FilePath> :t simpleFind (\p -> takeExtension p == ".c")
-- simpleFind (\p -> takeExtension p == ".c")
--   :: FilePath -> IO [FilePath]


result = do
    dir <- getCurrentDirectory
    r <- doesDirectoryExist dir
    if r then return True else return False
