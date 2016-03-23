
{-
 - We are going to write an invalid Monad and Functor.
 - And we are going to learn how to correct it so that its valid again.
 - Modules needed:
 -  (a) QuickCheck
 -  (b) checkers
 -  use 'cabal install QuickCheck' @version is 2.8.2
 -  use 'cabal install checkers'   @version is 0.4.4
 -}
module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = 
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)
  -- previously,
  -- fmap f (CountMe i a) = CountMe (i+1) (f a)
  -- violates the laws of identity since i =!= i+1

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n+n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f = 
    let CountMe n' b = f a
    in CountMe (n+n') b
  {-
   - violates left-id, right-id and composition 
   - laws; makes sense because of the "+1"
  CountMe n a >>= f = 
    let CountMe _ b = f a
    in CountMe (n+1) b
  -}
instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

main = do
  {-
   - "trigger" seems to be defined just for type-inferencing
   -}
  let trigger = undefined :: CountMe (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  -- the above is equivalent to those below
  quickBatch $ functor (undefined :: CountMe (Int, String, Int))
  quickBatch $ applicative (undefined :: CountMe (Int, String, Int))
  quickBatch $ monad (undefined :: CountMe (Int, String, Int))

