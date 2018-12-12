{-# LANGUAGE BangPatterns #-}

import System.IO
import System.IO.Error
import System.Environment
import Control.Exception
import Control.Concurrent

suboptimal = do
  fs <- getArgs
  let
    loop !n [] = return n
    loop !n (f:fs)
      = handle (\e -> if isDoesNotExistError e
                          then loop n fs
               else throwIO e) $
                 do
                   getMaskingState >>= print
                   h <- openFile f ReadMode
                   s <- hGetContents h
                   loop (n + length (lines s)) fs
  n <- loop 0 fs
  print n

optimal = do
  fs <- getArgs
  let
    loop !n [] = return n
    loop !n (f:fs) = do
      getMaskingState >>= print
      r <- Control.Exception.try (openFile f ReadMode)
      case r of
          Left e | isDoesNotExistError e -> loop n fs
            | otherwise -> throwIO e
          Right h -> do
            s <- hGetContents h
            loop (n + length (lines s)) fs
  n <- loop 0 fs
  print n

main = optimal

