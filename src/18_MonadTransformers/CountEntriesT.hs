module CountEntriesT (countEntries) where

import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO
import Control.Monad (liftM, forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, execWriterT)

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

-- main :: IO ()
-- main = do
--   putStrLn "Give me a directory name"
--   name <- getLine
--   putStrLn $ "About to process " ++ show name
--   (execWriterT $ countEntries name) `map` \filePath count -> putStrLn $ show (filePath)

