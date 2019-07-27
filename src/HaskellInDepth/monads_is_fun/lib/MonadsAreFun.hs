{-# LANGUAGE OverloadedStrings #-}

module MonadsAreFun (
  lookup,
  locByName,
  Config(..),
  ConfigM
  ) where

import Prelude hiding (lookup)
import Safe
import Control.Monad.Reader

type Name = String
type Phone = String
type Location = String
type PhoneNumbers = [(Name, Phone)]
type Locations = [(Phone, Location)]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup = undefined

locByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locByName pnumbers locs name = lookup name pnumbers >>= flip lookup locs

data Config = Config { key :: String, flag :: Bool, value :: String } deriving Show

type ConfigM = Reader Config

