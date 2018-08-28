
{-# LANGUAGE GeneralizedNewtypeDeriving,
             TypeSynonymInstances,
             FlexibleInstances,
             FunctionalDependencies,
             MultiParamTypeClasses #-}

import MonadHandle
import SafeHello
import System.IO (IOMode(..))
import Control.Monad.Writer

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
           deriving (Show)


newtype WriterIO a = W { runW :: Writer [Event] a } deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle FilePath WriterIO where
  openFile filePath mode = tell [Open filePath mode]  >> return filePath
  hPutStr h datum = tell [Put h datum]
  hClose h = tell [Close h]
  hGetContents h = tell [GetContents h] >> return ""

