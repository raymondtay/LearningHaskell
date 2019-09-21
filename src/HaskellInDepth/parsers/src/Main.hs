{-#LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8  (pack) -- bytestring package
import Data.Text.Format              -- text-format package
import Parse

main :: IO ()
main = do
  let invalid0 = "...."
      valid0 = "192.168.1.0"
      valid1 = "127.0.0.1"
      preamble = "Before {}, after: [{}]" :: Format
  putStrLn . show $ (format preamble (invalid0, show . parseIP $ pack (invalid0)))
  putStrLn . show $ (format preamble (valid0  , show . parseIP $ pack (valid0)))
  putStrLn . show $ (format preamble (valid1  , show . parseIP $ pack (valid1)))


