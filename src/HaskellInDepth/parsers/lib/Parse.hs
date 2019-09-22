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

module Parse(logParser, logFile, parseIP') where

import Data.ByteString       hiding (count)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Word
import Control.Applicative
import Data.Either

import Data.Time

logFile :: FilePath
logFile = "data.log"

data Product = Mouse | Keyboard | Speakers | Monitor deriving Show
data Source = Internet | Friend deriving Show

data LogEntry = 
  LogEntry {
    entryTime :: LocalTime, 
    entryIP :: IP,
    entryProduct :: Product,
    source :: Source
  } deriving Show

type Log = [LogEntry] -- list of log entries

timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d),
                localTimeOfDay = TimeOfDay (read h) (read m) (read s)
              }

-- Word8 represents 8-bit unsigned integer values
data IP = IP Word8 Word8 Word8 Word8 deriving Show

parseIP' :: ByteString -> IP
parseIP' datum = 
  let
    _parser = (sepBy1 (many1 decimal) (char '.'))
    parsedS = fromRight ([[0],[0],[0],[0]]::[[Word8]]) (parseOnly _parser datum)
  in IP (parsedS !! 0 !! 0) (parsedS !! 1 !! 0) (parsedS !! 2 !! 0) (parsedS !! 3 !! 0)

parseIP :: Parser IP
parseIP = do
  b1 <- decimal
  char '.'
  b2 <- decimal
  char '.'
  b3 <- decimal
  char '.'
  b4 <- decimal
  return $ IP b1 b2 b3 b4


productParser :: Parser Product
productParser =
  (string "mouse" >> return Mouse)
  <|> (string "keyboard" >> return Keyboard)
  <|> (string "monitor" >> return Monitor)
  <|> (string "speakers" >> return Speakers)

sourceParser :: Parser Source
sourceParser =
  (string "internet" >> return Internet) <|> (string "friend" >> return Friend)

logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  ip <- parseIP
  prd <- productParser
  src <- sourceParser
  return $ LogEntry t ip prd src

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

