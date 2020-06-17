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

bindir     = "/Users/tayboonl/Library/Haskell/bin"
libdir     = "/Users/tayboonl/Library/Haskell/ghc-8.4.3-x86_64/lib/monads-is-fun-0.1.0.0"
dynlibdir  = "/Users/tayboonl/Library/Haskell/ghc-8.4.3-x86_64/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/tayboonl/Library/Haskell/share/ghc-8.4.3-x86_64/monads-is-fun-0.1.0.0"
libexecdir = "/Users/tayboonl/Library/Haskell/libexec/x86_64-osx-ghc-8.4.3/monads-is-fun-0.1.0.0"
sysconfdir = "/Users/tayboonl/Library/Haskell/etc"

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
