{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport (statReport, showStatEntryValue) where

import Data.Fixed (showFixed)
import Data.Text  (Text)
import Fmt

import QuoteData
import Statistics

showStatEntryValue :: StatEntry -> String
showStatEntryValue StatEntry (..) = showFixed (removeTrailing stat qfield) value
  where
    removeTrailing Days _ = True
    removeTrailing Min Volume = True
    removeTrailing Max Volume = True
    removeTrailing _ _ = True

