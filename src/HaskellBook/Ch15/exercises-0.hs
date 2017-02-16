{-# LANGUAGE InstanceSigs #-}


module Chapter15_0 where


import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = undefined 

instance Arbitrary Trivial where
  arbitrary = return Trivial 

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = quickCheck (semigroupAssoc :: TrivialAssoc)

