module Paths_hello_world (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/raymondtay/LearningHaskell/src/misc/hello-world/.cabal-sandbox/bin"
libdir     = "/Users/raymondtay/LearningHaskell/src/misc/hello-world/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.3/hello-world-0.1.0.0-8KiVObsurkt5Lizzs5lAqO"
datadir    = "/Users/raymondtay/LearningHaskell/src/misc/hello-world/.cabal-sandbox/share/x86_64-osx-ghc-7.10.3/hello-world-0.1.0.0"
libexecdir = "/Users/raymondtay/LearningHaskell/src/misc/hello-world/.cabal-sandbox/libexec"
sysconfdir = "/Users/raymondtay/LearningHaskell/src/misc/hello-world/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hello_world_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hello_world_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hello_world_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hello_world_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hello_world_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
