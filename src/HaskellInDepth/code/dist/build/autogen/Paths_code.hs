{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_code (
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
libdir     = "/Users/tayboonl/Library/Haskell/ghc-8.6.5-x86_64/lib/code-0.1.0.0"
dynlibdir  = "/Users/tayboonl/Library/Haskell/ghc-8.6.5-x86_64/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tayboonl/Library/Haskell/share/ghc-8.6.5-x86_64/code-0.1.0.0"
libexecdir = "/Users/tayboonl/Library/Haskell/libexec/x86_64-osx-ghc-8.6.5/code-0.1.0.0"
sysconfdir = "/Users/tayboonl/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "code_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "code_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
