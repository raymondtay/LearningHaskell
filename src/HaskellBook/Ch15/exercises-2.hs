{-# LANGUAGE InstanceSigs #-}

module Chapter15_2 where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidRightIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidLeftIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Optional a = Nada | Only a deriving (Eq, Show)
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend :: Optional a -> Optional a -> Optional a
  mappend Nada _ = Nada
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (mappend a b)

newtype First' a = First' { getFirst' :: Optional a  } deriving (Eq, Show)

instance Monoid a => Monoid (First' a) where
  mempty = First' Nada
  mappend (First' a) (First' b) = First' (mappend a b)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FirstMappend)
  quickCheck (monoidRightIdentity :: FirstMappend)

