module CountEntries (listDirectory, countEntriesTrad) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO
import Control.Monad (forM, liftM)

-- Given a directory name, return all names found in the listing excluding "."
-- and ".." i.e. the current and parent directories
listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
                then countEntriesTrad newName
                else return []
  return $ (path, length contents) : concat rest

main = do
  putStrLn "Give me a directory name"
  name <- getLine
  putStrLn $ "About to process " ++ show name
  countEntriesTrad name

