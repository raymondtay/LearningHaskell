{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses #-}

import MonadHandle 
import qualified System.IO
import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import System.Directory (removeFile)
import Control.Monad.Writer

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world "
  hClose h

instance MonadHandle System.IO.Handle IO where
  openFile = System.IO.openFile
  hPutStr = System.IO.hPutStr
  hClose = System.IO.hClose
  hGetContents = System.IO.hGetContents
  hPutStrLn = System.IO.hPutStrLn

{-

  Isolation and Testing
  ========================

  In fact, because our safeHello function does not use the IO type, we can even use a monad
  that cannot perform I/O. This allows us to test code that would normally have side effects in a
  completely pure, controlled environment.

  To do this, we will create a monad that doesn't perform I/O but instead logs every file-related
  event for later processing.

-}

data Event = Open FilePath IOMode
            | Put String String
            | Close String
            | GetContents String deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a } deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event]) 
runWriterIO = runWriter . runW

