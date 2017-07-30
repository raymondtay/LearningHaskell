
module Recursivecontents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

--
-- Iterate through the contents of the directory in question and then
-- for each name we find in there, we perform a check and determine if its
-- a directory to which we would call `getRecursiveContents` else we simply
-- stop at that directory-level in the filetree and wind-up the stack.
--
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topDir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
        then getRecursiveContents path
        else return [path]
  return (concat paths)

