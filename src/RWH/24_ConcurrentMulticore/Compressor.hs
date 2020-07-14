
module Compressor where

-- suppose we have a large file to compress and write to disk, but we want to
-- handle a user's input quickly enough that she will perceive our program as
-- responding immediately. If we use `forkIO` to write the file out in a
-- separate thread, we can do both simultaneously.

{-|
  | On OSX, i have already installed `brew` and inorder to get the latest readline haskell
  | package i have to do this and the rationale is because the options `with-readline-includes`
    is a parameter that is understood by the package so you have to tell cabal to pass it along and not interpret it.
  | cabal install readline --extra-include-dirs=/usr/local/Cellar/readline/7.0.5/include --extra-lib-dirs=/usr/local/Cellar/readline/7.0.5/lib --configure-option=--with-readline-includes=/usr/local/Cellar/readline/7.0.5/include --configure-option=--with-readline-libraries=/usr/local/Cellar/readline/7.0.5/lib
  |-}
import Control.Concurrent (forkIO)
import Control.Exception (handle, SomeException)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import System.Console.Readline (readline)

-- provided by the 'zlib' package on http://hackage.haskell.org/
import Codec.Compression.GZip (compress)

main = do
  maybeLine <- readline "Enter a file to compress> "
  case maybeLine of
    Nothing -> return () -- user entered EOF
    Just "" -> return () -- treat no name as "want to quit"
    Just name -> do
         handle (print :: SomeException -> IO ()) $ do
           content <- L.readFile name
           forkIO (compressFile name content)
           return ()
         main
  where compressFile path = L.writeFile (path ++ ".gz") . compress

