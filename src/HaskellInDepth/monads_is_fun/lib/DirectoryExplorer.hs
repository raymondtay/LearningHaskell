{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module DirectoryExplorer where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS
import Control.Monad.ST
import Safe
import System.Directory
import System.FilePath
import Data.Foldable      (traverse_)


-- The application consists of the following components:
-- => the configuration containing base path, maximal depth of reported
-- subdirectoris, and optional extension for filtering files;
-- => mutable state for traversing subdirectories with the current depth, the
-- current path and one unspecified field that can be used for any purpose by a
-- concrete application;
-- => and log with entries of the path and computed information for that path
--

data AppConfig = AppConfig {
basePath :: FilePath,
maxDepth :: Int,
ext :: Maybe FilePath
                           }

data AppState s = AppState {
curDepth :: Int,
curPath  :: FilePath,
st_field :: s
                           }

type AppLog s = [(FilePath, s)]

type MyApp s = RWST AppConfig (AppLog s) (AppState s) IO

runMyApp :: MyApp s a -> AppConfig -> s -> IO (a, AppLog s)
runMyApp app config init =
  evalRWST app config (AppState 0 (basePath config) init)

traverseDirectoryWith :: MyApp s () -> MyApp s ()
traverseDirectoryWith app = do
  path <- gets curPath
  content <- liftIO $ listDirectory path
  traverse_ (go path) content
    where 
      go path name = do
        modify (newPath $ path </> name)
        app
        modify (restorePath $ path)
      newPath path st @ AppState {..} = st { curDepth = curDepth +1, curPath = path }
      restorePath path st @ AppState {..} = st { curDepth = curDepth - 1, curPath = path }


