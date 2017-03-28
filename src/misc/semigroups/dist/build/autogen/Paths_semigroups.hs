module Paths_semigroups (
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

bindir     = "/Users/raymondtay/Library/Haskell/bin"
libdir     = "/Users/raymondtay/Library/Haskell/ghc-7.10.3-x86_64/lib/semigroups-0.1.0.0"
datadir    = "/Users/raymondtay/Library/Haskell/share/ghc-7.10.3-x86_64/semigroups-0.1.0.0"
libexecdir = "/Users/raymondtay/Library/Haskell/libexec"
sysconfdir = "/Users/raymondtay/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "semigroups_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "semigroups_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "semigroups_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "semigroups_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "semigroups_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
