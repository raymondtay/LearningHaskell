{-# LANGUAGE FunctionalDependencies,
             MultiParamTypeClasses 
#-}
import SimpleMPTC

import qualified System.IO
import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))

import SimpleMPTCDemo

instance Base System.IO.Handle IO where
  hPutStr = System.IO.hPutStr
  openFile = System.IO.openFile
  hClose   = System.IO.hClose

