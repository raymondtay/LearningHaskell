module Chap15 where

import MyOptional -- see `MyOptional.hs`

newtype First' a = 
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance (Monoid a) => Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) _ = (First' Nada)
  mappend _ (First' Nada) = (First' Nada)
  mappend (First' l) (First' r) = (First' $ mappend l r)

firstMappend :: (Monoid a) => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
  First' String
  -> First' String
  -> First' String
  -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First's String -> Bool)
  quickCheck (monoidRightIdentity :: First's String -> Bool)
