{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Basics2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tayboonl/Library/Haskell/bin"
libdir     = "/Users/tayboonl/Library/Haskell/ghc-8.6.5-x86_64/lib/Basics2-0.1.0.0"
dynlibdir  = "/Users/tayboonl/Library/Haskell/ghc-8.6.5-x86_64/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tayboonl/Library/Haskell/share/ghc-8.6.5-x86_64/Basics2-0.1.0.0"
libexecdir = "/Users/tayboonl/Library/Haskell/libexec/x86_64-osx-ghc-8.6.5/Basics2-0.1.0.0"
sysconfdir = "/Users/tayboonl/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Basics2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Basics2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Basics2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Basics2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Basics2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Basics2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
