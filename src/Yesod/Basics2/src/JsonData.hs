{-# LANGUAGE DeriveGeneric #-}

module JsonData where

import GHC.Generics
import Data.Text

data Person =
  Person {
    name :: Text,
    age :: Int
  } deriving (Generic, Show)


