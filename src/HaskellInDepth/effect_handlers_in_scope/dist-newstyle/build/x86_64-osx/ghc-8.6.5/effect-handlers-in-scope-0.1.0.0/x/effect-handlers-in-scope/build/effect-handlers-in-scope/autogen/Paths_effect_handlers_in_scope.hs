{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_effect_handlers_in_scope (
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
libdir     = "/Users/tayboonl/.cabal/lib/x86_64-osx-ghc-8.6.5/effect-handlers-in-scope-0.1.0.0-inplace-effect-handlers-in-scope"
dynlibdir  = "/Users/tayboonl/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tayboonl/.cabal/share/x86_64-osx-ghc-8.6.5/effect-handlers-in-scope-0.1.0.0"
libexecdir = "/Users/tayboonl/.cabal/libexec/x86_64-osx-ghc-8.6.5/effect-handlers-in-scope-0.1.0.0"
sysconfdir = "/Users/tayboonl/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "effect_handlers_in_scope_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "effect_handlers_in_scope_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "effect_handlers_in_scope_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "effect_handlers_in_scope_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "effect_handlers_in_scope_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "effect_handlers_in_scope_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
