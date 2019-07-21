{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- The haskell package "fmt" is interesting and illustrates the power of
-- haskell as a DSL
--
module StatReport (statReport, showStatEntryValue) where

import Data.Fixed (showFixed)
import Data.Text  (Text)
import Fmt

import QuoteData
import Statistics

showStatEntryValue :: StatEntry -> String
showStatEntryValue StatEntry {..} = showFixed (removeTrailing stat qfield) value
  where
    removeTrailing Days _ = True
    removeTrailing Min Volume = True
    removeTrailing Max Volume = True
    removeTrailing _ _ = True

instance Buildable StatQFieldData where
  build (qf, stats) = nameF ("Statistics for " +||qf||+"") $ unlinesF stats

instance Buildable StatEntry where
  build se@StatEntry {..} = ""+|stat|+": "+|showStatEntryValue se|+""

statReport :: StatInfo -> Text
statReport = fmt . unlinesF

