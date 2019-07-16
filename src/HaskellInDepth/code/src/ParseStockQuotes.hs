{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Data.Fixed
import Data.Text as T
import Data.Text.IO as TIO
import System.Environment

data E4 -- empty data type

instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4 -- values are truncated instead of rounded

-- parsing data
data QuoteData =
  QuoteData {
      day :: Day,
      close :: Fixed4,
      volume :: Fixed4,
      open :: Fixed4,
      high :: Fixed4,
      low :: Fixed4
  } deriving Show

main :: IO ()
main = do
  [fname] <- getArgs
  ds <- TIO.readFile fname
  forM_ (text2Quotes ds) (\rec -> Prelude.putStrLn $ show rec)
  return ()

-- Text data to Quote data
-- 
text2Quotes :: T.Text -> [QuoteData]
text2Quotes = Prelude.map (mkQuote . toComponents) . Prelude.tail . T.lines
  where
    toComponents = Prelude.map T.unpack . T.splitOn ","
    mkQuote (d : rest @ [_, _, _, _, _]) = 
      let day = parseTimeOrError False defaultTimeLocale "%Y/%m/%d" d
          [close, volume, open, high, low] = Prelude.map read rest in QuoteData {..}
    mkQuote _ = error "Was not expecting this other format"



