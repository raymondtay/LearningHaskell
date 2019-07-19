{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Statistics (Statistic(..), StatEntry(..), StatQFieldData, StatInfo, statInfo) where

import Fmt
import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)
import BoundedEnum
import QuoteData

data Statistic = Mean | Min | Max | Days deriving (Show, Eq, Enum, Bounded, BoundedEnum)

data StatEntry = StatEntry { stat :: Statistic, qfield :: QField, value :: Fixed4 }

type StatQFieldData = (QField, [StatEntry])

type StatInfo = [StatQFieldData]

instance Buildable Statistic where
  build Mean = "Mean"
  build Max = "Maximum"
  build Min = "Minimum"
  build Days = "Days Between Min/Max"













instance Buildable StatQFieldData where
  build (qf, stats) = nameF ("Statistics for " +||qf||+"") $ unlinesF stats

instance Buildable StatEntry where
  build se@StatEntry {..} = ""+|stat|+": "+|showStatEntryValue se|+""

mean xs = sum xs / fromIntegral (length xs)

daysBetween qf quotes = fromIntegral $ abs $ diffDays dMinQuote dMaxQuote
  where
    cmp = comparing (field2fun qf)
    dMinQuote = day $ minimumBy cmp quotes
    dMaxQuote = day $ maximumBy cmp quotes

funcByField func qf = func . fmap (field2fun qf)

computeStatistic Mean = funcByField mean
computeStatistic Min = funcByField minimum
computeStatistic Max = funcByField maximum
computeStatistic Days = daysBetween

statInfo :: (Functor t, Foldable t) => t QuoteData -> StatInfo
statInfo quotes = map stQFData range
  where stQFData qf = (qf, [ StatEntry st qf v | st <- range, let v = computeStatistic st qf quotes ])
