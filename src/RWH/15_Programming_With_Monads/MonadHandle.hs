
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module MonadHandle (MonadHandle(..)) where

import System.IO (IOMode(..))

class Monad m => MonadHandle h m | m -> h where
  openFile :: FilePath -> IOMode -> m h
  hPutStr :: h -> String -> m ()
  hClose :: h -> m ()
  hGetContents :: h -> m String

  hPutStrLn :: h -> String -> m () -- cascading 2 IO actions is totally different from `liftIO`, in case you were wondering
  hPutStrLn h s = hPutStr h s >> hPutStr h "\n"

-- Here we have chosen to abstract away both the type of the monad and the type
-- of a file handle. To satisfy the type checker, we have added a functional
-- dependency: for any instance of MonadHandle, there is exactly one handle
-- type that we can use. When we make the IO monad an instance of this class,
-- we use a regular Handle:
--



