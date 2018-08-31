{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

import MonadHandle
import qualified System.IO
import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import System.Directory (removeFile)

import SafeHello

instance MonadHandle System.IO.Handle IO where
  openFile     = System.IO.openFile
  hPutStr      = System.IO.hPutStr
  hClose       = System.IO.hClose
  hGetContents = System.IO.hGetContents
  hPutStrLn    = System.IO.hPutStrLn

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h


