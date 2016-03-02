{-# LANGUAGE DatatypeContexts #-}

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import MyOptional 

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity a = (a <> mempty) == a
monoidRightIdentity a = (mempty <> a) == a

newtype (Num a) => First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)
instance (Num a) => Monoid (First' a ) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' (Only a)) (First' Nada) = First' (Only a)
  mappend (First' Nada) (First' (Only a)) = First' (Only a)
  mappend (First' (Only b)) (First' (Only a)) = First' (Only $ getSum $ (+) (Sum a) (Sum b))
 
firstMappend :: (Num a) => First' a -> First' a -> First' a
firstMappend = mappend
 
data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency[ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where 
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)
  --monoidAssoc firstMappend
  --monoidLeftIdentity mempty firstMappend
  --monoidRightIdentity mempty firstMappend

