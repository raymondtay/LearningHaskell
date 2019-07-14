{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module QuoteData where

import Data.Fixed (HasResolution(..), Fixed)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Safe (readDef)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField(..))
import BoundedEnum

data E4
instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4

data QuoteData = QuoteData {
  day :: Day,
  close :: Fixed4,
  volume :: Fixed4,
  open :: Fixed4,
  high :: Fixed4,
  low :: Fixed4 } deriving (Generic, FromNamedRecord)


-- FromField typeclass defines how to parse field of type a. The type Field is
-- in fact a synonym for ByteString (that's why we need to use unpack) and
-- Parser is a monad for parsing used inside cassava.
instance FromField Fixed4 where
  parseField = pure . readDef 0 . unpack
instance FromField Day where
  parseField s = parseTimeM False defaultTimeLocale "%Y/%m/%d" (unpack s)

data QField = Open | Close | High | Low | Volume deriving (Show, Enum, Bounded, BoundedEnum)
field2fun :: QField -> QuoteData -> Fixed4
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = volume


