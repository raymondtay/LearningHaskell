module Chap19 where

import Data.Time.Clock

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset = 
  fmap (addUTCTime (offset * 24 * 3600)) $ getCurrentTime

