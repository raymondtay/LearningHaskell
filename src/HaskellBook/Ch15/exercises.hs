{-# LANGUAGE InstanceSigs #-}

-- InstanceSigs allows me to embed the type signature
-- into the function s.t. it helps me "See" better.
--
module Chapter15 where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
data Bull = Fools | Twoo deriving (Eq, Show)
instance Arbitrary Bull where
  arbitrary = frequency[(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidRightIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidLeftIdentity :: (Eq m, Monoid m ) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

data Optional a = Nada | Only a deriving (Eq, Show)

-- 
-- Definition of monoid for Optional
--
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend :: Optional a -> Optional a -> Optional a
  mappend Nada _ = Nada
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (mappend a b)

-- *Chapter15 Data.List Data.Monoid> :t (<>)
-- (<>) :: Monoid m => m -> m -> m
-- *Chapter15 Data.List Data.Monoid> Only (Sum 1) <> Nada
-- Only (Sum {getSum = 1})
-- *Chapter15 Data.List Data.Monoid> Only (Sum 1) <> Only (Sum 4)
-- Only (Sum {getSum = 5})
-- *Chapter15 Data.List Data.Monoid> Nada <> Only (Sum 1)
-- Nada
--






























