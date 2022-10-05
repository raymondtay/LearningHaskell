{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Fmt
import Data.Ord
import Data.Char
import Data.List (group, sort, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

type Entry = (T.Text, Int) -- one vocabulary entry
type Vocabulary = [Entry] -- list of entries

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws =  map T.toCaseFold  $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x: _) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

allWords :: Vocabulary -> [T.Text]
allWords vocab = map fst vocab

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocab = (,) (foldl (\acc -> \(_,x) -> acc + x) 0 vocab) $ length vocab

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  printAllWords vocab

processTextFile' :: FilePath -> Bool -> Int -> IO ()
processTextFile' fname withAllWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab = fmt $
  "Total number of words: " +|total|+ 
    " Number of unique words: " +|unique|+ "\n"
      where
        (total, unique) = wordsCount vocab

allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab =
  fmt $ nameF "All words" $ unlinesF (allWords vocab)

frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab num =
  fmt $ nameF "Frequent words" $ blockListF' "" fmtEntry reportData
    where
      reportData = take num $ wordsByFrequency vocab
      fmtEntry (t, n) = ""+|t|+": "+|n|+""

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] -> processTextFile' fname True (read num)
    [fname, num] -> processTextFile' fname False (read num)
    _ -> putStrLn "Usage: vocab-builder [-a] filename freq_words_num"

-- main :: IO ()
-- main = do
--   [fname] <- getArgs
--   text <- TIO.readFile fname
--   let ws = map head $ group $ sort $ map T.toCaseFold $ filter (not . T.null) $ map (T.dropAround $ not . isLetter) $ T.words text
--   TIO.putStrLn $ T.unwords ws
--   print $ length ws

