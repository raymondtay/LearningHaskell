{-# LANGUAGE OverloadedStrings, RecordWildCards #-}


module Main where


import Control.Monad
import Data.Time
import Data.Fixed
import Data.Text as T
import Data.Text.IO as TIO
import System.Environment
import qualified Data.ByteString.Lazy as BL (readFile)
import Data.Csv (decodeByName)


import QuoteData
import Statistics
import StatReport
import Charts
import Params

main :: IO ()
main = cmdLineParser >> work

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes) -> generateReports params quotes

-- main :: IO ()
-- main = do
--   [fname] <- getArgs
--   ds <- TIO.readFile fname
--   forM_ (text2Quotes ds) (\rec -> Prelude.putStrLn $ show rec)
--   return ()

-- Text data to Quote data
-- 
-- text2Quotes :: T.Text -> [QuoteData]
-- text2Quotes = Prelude.map (mkQuote . toComponents) . Prelude.tail . T.lines
--   where
--     toComponents = Prelude.map T.unpack . T.splitOn ","
--     mkQuote (d : rest @ [_, _, _, _, _]) = 
--       let day = parseTimeOrError False defaultTimeLocale "%Y/%m/%d" d
--           [close, volume, open, high, low] = Prelude.map read rest in QuoteData {..}
--     mkQuote _ = error "Was not expecting this other format"


