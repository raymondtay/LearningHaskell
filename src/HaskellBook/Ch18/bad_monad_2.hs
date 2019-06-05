{-#LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a deriving (Show, Eq)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

instance Arbitrary (CountMe Int) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

--
-- Basics of creating tests to validate the laws of a typeclass or datatype
-- created.
--
main = do
  let trigger = undefined :: CountMe (Int, Int, Int)
  quickBatch $ functor trigger 
