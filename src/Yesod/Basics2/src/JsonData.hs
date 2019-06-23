{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module JsonData where

import Data.Aeson
import GHC.Generics
import Data.Text

data Person =
  Person {
    name :: Text,
    age :: Int
  } deriving (Generic, Show)

-- Aeson would like me to provide instances on how to encode and decode
-- JSON to/fro the value objects.
instance ToJSON Person where
  toJSON (Person name age) = object ["name" .= name, "age" .= age]

instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person <$> v .: "name" <*> v .: "age"

