{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

-- The basic idea of a type family is to state some association between two
-- different types. Suppose we want to write a afunction that will safely take
-- the first element of a list. but we do not want it to work just on lists, we
-- would like it o treat a ByteString like a list of Word8s. To do so, we need
-- to introduce some associated type to specify what the contents of a certain
-- type are.
--
module Example where

import Data.Word (Word8)
import qualified Data.ByteString as S
import Data.ByteString.Char8 () -- get an orphan IsTring instance

class SafeHead a where
  type Content a
  safeHead :: a -> Maybe (Content a)

instance SafeHead [a] where
  type Content [a] = a
  safeHead [] = Nothing
  safeHead (x:_) = Just x

instance SafeHead S.ByteString where
  type Content S.ByteString = Word8
  safeHead bs
    | S.null bs = Nothing
    | otherwise = Just $ S.head bs

main :: IO ()
main = do
  print $ safeHead ("" :: String)
  print $ safeHead ("hi" :: String)
  print $ safeHead ("" :: S.ByteString)
  print $ safeHead ("world" :: S.ByteString)
