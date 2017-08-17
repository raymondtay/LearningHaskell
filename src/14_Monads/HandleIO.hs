
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO(
  HandleIO,
  Handle,
  IOMode(..),
  runHandleIO,
  openFile,
  hClose,
  hPutStrLn) where

import Control.Monad.Trans (MonadIO(..))
import System.IO (Handle, IOMode(..))
import System.Directory (removeFile)
import qualified System.IO


newtype HandleIO a = HandleIO { runHandleIO :: IO a } deriving (Functor, Applicative, Monad)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO ( System.IO.hPutStrLn h s )

safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hi there!"
  hClose h

{-
  On Page 377 of the RWH book, the passage reads 
  "There's one small, but significant, problem with our HandleIO monad: it does not take into
  account the possibility that we might occasionally need an escape hatch. If we define a monad
  such as this, it is likely that we will occasionally need to perform an I/O aciton that isn't 
  allowed for by the design of our monad.
  "
-}

instance MonadIO HandleIO where
  liftIO = HandleIO

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)
  
