{-#LANGUAGE OverloadedStrings #-}

module Main where

import System.IO                             (print)
import qualified Data.ByteString.Char8  as B (readFile,unpack,pack) -- bytestring package
import Data.Attoparsec.ByteString
import Data.Text
import Data.Text.Format              -- text-format package
import Parse


main :: IO ()
main = do
  let invalid0 = "...."
      valid0 = "192.168.1.0"
      valid1 = "127.0.0.1"
      preamble = "Before {}, after: [{}]" :: Format
  putStrLn . show $ (format preamble (invalid0, show . parseIP' $ B.pack (invalid0)))
  putStrLn . show $ (format preamble (valid0  , show . parseIP' $ B.pack (valid0)))
  putStrLn . show $ (format preamble (valid1  , show . parseIP' $ B.pack (valid1)))
  putStrLn "=> Parsing begins..."
  B.readFile logFile >>= System.IO.print . parseOnly logParser
  putStrLn "=> Parsing ended."

