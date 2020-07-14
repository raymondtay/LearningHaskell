
module CountEntries (listDirectory,countEntriesTrad) where

import System.Directory (doesDirectoryExist, getDirectoryContents) 
import System.FilePath ((</>))
import Control.Monad (forM, forM_, when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)

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



-- The normal `Writer` monad has two type parameters, so it's more properly
-- written `Writer w a`. The first parameter 'w' is the type of the values to
-- be recorded, and 'a' is the usual type that the `Monad` typeclass requires.
--
-- The `WriterT` transformer has a similar structure, but it adds another type
-- parameter 'm' : this is the underlying monad whose behavior we are
-- augmenting. The full signature of `WriterT` is `WriterT w m a`.
--

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName


