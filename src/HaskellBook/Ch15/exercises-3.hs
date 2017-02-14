{-# LANGUAGE InstanceSigs #-}

module Chapter15_3 where

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


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = undefined

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)


