{-# LANGUAGE OverloadedStrings #-}

{-
  Some time ago, i cannot remember when exactly, but i wanted to learn how to use the attoparsec[3]
  library for a parsing exercise to compare with haoyi's fastparse[2] library. The comparison was
  purely to understand the various approaches took by their individual creators and wasn't really
  about just considering the performance aspect.


  Reference:
  [1] - https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
  [2] - https://www.lihaoyi.com/fastparse/
  [3] - http://hackage.haskell.org/package/attoparsec-0.13.2.3
-}

module Parse(parseIP) where

import Data.ByteString
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Word
import Control.Applicative
import Data.Either


-- Word8 represents 8-bit unsigned integer values
data IP = IP Word8 Word8 Word8 Word8 deriving Show

parseIP :: ByteString -> IP
parseIP datum = 
  let
    _parser = (sepBy1 (many1 decimal) (char '.'))
    parsedS = fromRight ([[0],[0],[0],[0]]::[[Word8]]) (parseOnly _parser datum)
  in IP (parsedS !! 0 !! 0) (parsedS !! 1 !! 0) (parsedS !! 2 !! 0) (parsedS !! 3 !! 0)



