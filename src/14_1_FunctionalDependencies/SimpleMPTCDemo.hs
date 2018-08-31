{-# LANGUAGE FlexibleContexts #-}

module SimpleMPTCDemo where

import System.IO (stdout, IOMode(..),Handle(..))
import SimpleMPTC

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Codec.Compression.GZip as GZip

sayHelloToStdOut :: Base System.IO.Handle m => m ()
sayHelloToStdOut = do
  hPutStrLn stdout "hello world"

sayHelloToFile :: Base h m => FilePath -> m ()
sayHelloToFile path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h

-- Use the compressed version of the file
sayHelloToFileWithCompression :: Base h m => FilePath -> m ()
sayHelloToFileWithCompression path = do
  h <- openFile path WriteMode
  cPutStrLn h "hello world"
  hClose h

readCompressedFile path = do
  d <- readFile path
  return (B.unpack . GZip.decompress . B.pack $ d)

-- The beauty of the typeclass approach is that we can swap one underlying
-- monad for another without touching much code, as most of our code does not
-- know or care about the implementation. For instance, we could replace IO
-- with a monad that compresses files as it writes them out.
--

