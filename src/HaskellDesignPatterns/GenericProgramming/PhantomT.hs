{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

--
-- Phantom Types and its applications
--
module PhantomT (sanitise, formData, validate, useData, dataToUpper) where

import Data.Char

data FormData a = FormData String

changeType :: FormData a -> FormData b
changeType (FormData str) = FormData str

data Validated
data Unvalidated

formData :: String -> FormData Unvalidated
formData str = FormData str

sampleData :: String
sampleData = "this is a really cool datum which has absolutely no use"

validate :: FormData Unvalidated -> Maybe (FormData Validated)
validate (FormData str) = case str of
  "Give it to me" -> Just (FormData sampleData)
  "GIVE IT TO ME" -> Just . dataToUpper $ (FormData sampleData)
  _     -> Nothing

useData :: FormData Validated -> IO ()
useData (FormData str) = putStrLn . show $ str

-- Th beauty of this is that we can define functions that work on all kinds of
-- `FormData` but still cannot turn unvalidated data into validated data

liftStringFn :: (String -> String) -> FormData a -> FormData a
liftStringFn f (FormData s) = FormData (f s)

dataToUpper :: FormData a -> FormData a
dataToUpper = liftStringFn (map toUpper)

class Sanitise a where
  sanitise :: FormData a -> FormData Validated

-- do nothing to data that is already validated
instance Sanitise Validated where
  sanitise = id

instance Sanitise Unvalidated where
  sanitise (FormData s) = FormData (filter isAlpha s)


