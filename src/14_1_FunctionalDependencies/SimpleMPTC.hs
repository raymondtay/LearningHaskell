{-# LANGUAGE FunctionalDependencies,
             MultiParamTypeClasses ,
             AllowAmbiguousTypes
#-}

module SimpleMPTC (Base(..)) where

-- Based on what i read from Real World Haskell, it does resemble a lot like
-- the class overloading concepts you find in Java (or even in C++); when i use
-- the term "resemble" i mean it "looks a lot like"...
--

import Control.Monad
import System.IO (IOMode(..))
import Control.Monad.Trans

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Codec.Compression.GZip as GZip

-- The idea of "m uniquely determines h" has no implications and take notice
-- that hPutStr has no default implementation at all.
-- Next question: can i provide a default implementation here and "override" it
-- on the instance level?
-- Next question: I actually wanted to write a function like this:
-- compress :: String -> String
-- compress = B.unpack . GZip.compress . B.pack
--
class Monad m => Base h m | m -> h where
  openFile :: FilePath -> IOMode -> m h
  hClose :: h -> m ()
  hPutStr :: h -> String -> m ()

  compress :: String -> String
  compress = B.unpack . GZip.compress . B.pack

  hPutStrLn :: h -> String -> m ()
  hPutStrLn h s = hPutStr h s >> hPutStr h "\n"
  
  cPutStrLn :: h -> String -> m ()
  cPutStrLn h s = hPutStr h (B.unpack . GZip.compress . B.pack $ s) >> hPutStr h "\n"


