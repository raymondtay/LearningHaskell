{-#LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

type Entry = (T.Text, Int) -- one entry 
type Vocabulary = [Entry]


--
-- Run: cabal run -- texts/hamlet.txt
--
main = do
  args <- getArgs
  case args of 
    [fname, n] -> processTextFile fname (read n)
    _ -> putStrLn "Usage: vocab-builder filename"

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry ws@(w:_) = (w, length ws)
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab


wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab n = T.append "\nFrequent words:\n"
                              $ T.unlines $ map showEntry $ take n
                              $ wordsByFrequency vocab
                                where 
                                  showEntry (t, n) = T.append t $ T.pack $ " - " ++ show n
wordsCount :: Vocabulary -> Int
wordsCount vocab = sum $ map snd vocab

wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab = T.append "\nTotal number of words: " $ T.pack $ show $ wordsCount vocab

allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab = T.append "\nAll words: " $ T.unlines $ map fst vocab

processTextFile :: FilePath -> Int -> IO ()
processTextFile fname n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n


