{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module DirectoryExplorer where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS
-- import Control.Monad.Trans.RWS
import Control.Monad.ST
import Safe
import System.Directory
import System.FilePath
import Data.Foldable      (traverse_)
import System.Posix.Types (FileOffset)
import System.Posix.Files (isRegularFile, isDirectory, getFileStatus, fileSize, FileStatus)
import Options.Applicative

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
ext :: Maybe FilePath -- aka file extension
                           }

data AppState s = AppState {
curDepth :: Int,
curPath  :: FilePath,
st_field :: s -- in practical terms, this field is used in this example to store the accumulated size of the discovered files.
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

-- As we need to compute the sum of the file sizes and they are best described
-- with the FileOffset type (which normally corresponds to Int64)
type DUApp = MyApp FileOffset


-- Recording one file entry iff we are interested to 
recordEntry fp fs = do
  ext <- asks ext
  when (needRec fp ext $ isRegularFile fs) (addToTotalSize $ fileSize fs)
    where
      addToTotalSize :: FileOffset -> DUApp ()
      addToTotalSize ofs = modify (\st -> st { st_field = st_field st + ofs })
      needRec _ Nothing _ = True
      needRec fp (Just ext) isFile = isFile && (isExtensionOf ext fp)

-- logs the total space used by one directory
logDiffTotalSize :: FileOffset -> DUApp ()
logDiffTotalSize ts = do
  AppState {..} <- get
  tell [(curPath, st_field - ts)]


-- Complete the implementation of the computation of file space usage with the
-- diskUsage function
diskUsage :: DUApp ()
diskUsage = do
  maxDepth <- asks maxDepth
  AppState {..} <- get
  fs <- liftIO $ getFileStatus curPath
  let isDir = isDirectory fs
      shouldLog = isDir && curDepth <= maxDepth
  when isDir $ traverseDirectoryWith diskUsage
  recordEntry curPath fs
  when shouldLog $ logDiffTotalSize st_field

mkConfig :: Parser AppConfig
mkConfig =
  AppConfig <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault)
  <*> option auto (metavar "DEPTH" <> short 'd' <> long "depth" <> value 0 <> showDefault <> help "Maximum depth of reporting") 
  <*> optional (strOption (metavar "EXT" <> long "extension" <> short 'e' <> help "Filter files by extension"))

printLog :: Show s => AppLog s -> IO ()
printLog = traverse_ printEntry
  where
    printEntry (fp, s) = do
      putStr $ show s ++ "\t"
      putStrLn fp

workIt :: AppConfig -> IO ()
workIt config = do
  (_, xs)  <- runMyApp diskUsage config 0
  (_, xs') <- runMyApp fileCount config 0
  putStrLn "File counter: "
  printLog xs'
  putStrLn "File space usage: "
  printLog xs

doItNow :: IO () -- see Main.hs
doItNow = execParser opts >>= workIt
  where 
    opts = info (mkConfig <**> helper) (fullDesc <> progDesc "File space usage info")



fileCount :: MyApp Int ()
fileCount = do
  AppState {..} <- get
  fs <- liftIO $ getFileStatus curPath
  when (isDirectory fs) $ do
    AppConfig {..} <- ask
    when (curDepth <= maxDepth) $ traverseDirectoryWith fileCount 
    files <- liftIO $ listDirectory curPath
    tell [(curPath, length $ filterFiles ext files)]
  where
    filterFiles Nothing = id
    filterFiles (Just ext) = filter (isExtensionOf ext)


