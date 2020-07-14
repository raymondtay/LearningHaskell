

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

import Base

import qualified System.IO
import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))

instance MyBase System.IO.Handle IO where
  write = System.IO.hPutStr

