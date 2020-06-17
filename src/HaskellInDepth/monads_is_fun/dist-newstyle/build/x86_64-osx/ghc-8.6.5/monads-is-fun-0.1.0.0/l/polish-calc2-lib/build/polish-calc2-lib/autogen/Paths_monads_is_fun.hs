{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_monads_is_fun (
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

bindir     = "/Users/tayboonl/.cabal/bin"
libdir     = "/Users/tayboonl/.cabal/lib/x86_64-osx-ghc-8.6.5/monads-is-fun-0.1.0.0-inplace-polish-calc2-lib"
dynlibdir  = "/Users/tayboonl/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tayboonl/.cabal/share/x86_64-osx-ghc-8.6.5/monads-is-fun-0.1.0.0"
libexecdir = "/Users/tayboonl/.cabal/libexec/x86_64-osx-ghc-8.6.5/monads-is-fun-0.1.0.0"
sysconfdir = "/Users/tayboonl/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monads_is_fun_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monads_is_fun_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monads_is_fun_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monads_is_fun_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monads_is_fun_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monads_is_fun_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
