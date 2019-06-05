{-#LANGUAGE FlexibleInstances #-}

-- This is probably the implementation where i have the Monad typeclass
-- for `CountMe`. See `bad_monad_3.hs` for the Applicative & Functor
--
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Semigroup ((<>))

data CountMe a = CountMe Integer a deriving (Show, Eq)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

-- considering that (<>) aka mappend has various possible operations in the
-- realm of math, i have decided that in this source's definition it would take
-- the form of addition.
instance Semigroup Integer where
  (<>) i j = i + j

instance Applicative CountMe where
  pure = CountMe 0
  (<*>) (CountMe i f) (CountMe j b) = CountMe ((<>) i j) (f b)

instance Monad CountMe where
  return = CountMe 0
  (CountMe i a) >>= f =
    let CountMe _ x = f a
    in CountMe i x

instance Arbitrary (CountMe Int) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Arbitrary (CountMe (Int -> Int)) where
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
  quickBatch $ applicative trigger 
  quickBatch $ monad trigger 


