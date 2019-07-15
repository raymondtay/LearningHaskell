{-# LANGUAGE RecordWildCards #-}

import Data.Time
import Data.Fixed

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
      low:: Fixed4
  }

main :: IO ()
main = do
  return ()
