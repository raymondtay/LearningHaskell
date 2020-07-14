module MyRefresh  where

import Control.Monad (liftM, liftM2)

-- Preparation for Haskell talk
-- Date is TBD.
--
import Test.QuickCheck

-- ok example
prop_Revrev_ok xs = reverse (reverse xs) == xs
  where types = xs :: [Int]

prop_Revrev_ko xs = reverse xs == xs
  where types = xs :: [Int]

{-
    *MyRefresh> quickCheck prop_Revrev_ok
    +++ OK, passed 100 tests.
    *MyRefresh> quickCheck prop_Revrev_ko
    *** Failed! Falsifiable (after 4 tests and 2 shrinks):
    [1,0]
-}

-- | Conditional Properties
-- | <condition> ==> <property>
--
ordered xs = and (zipWith (<=) xs (drop 1 xs))

insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

prop_Insert x xs = ordered xs ==> ordered (insert x xs)
  where types = x :: Int


-- | Quantified Properties
-- | forAll <generator> $ \<pattern> -> <property>
--
prop_Insert2 x = forAll orderedList $ \xs -> ordered (insert x xs)
  where types = x :: Int

-- | Counting trivial cases (not supported in GHC 8.4.3)
-- | <condition> `trivial` <property>
--
-- prop_Insert3 x xs = ordered xs ==> null xs `trivial` ordered (insert x xs)
  -- where types = x :: Int

-- | Classifying Test Cases
-- | A property may take the form:
-- | 
-- | classify <condition> <string> $ <property>
--
prop_Insert4 x xs =
  ordered xs ==>
    classify (ordered (x:xs)) "at-head"$
    classify (ordered (xs++[x])) "at-tail"$
    ordered (insert x xs)
      where types = x :: Int

-- | Collecting Data Values
-- | A property may take the form:
-- | collect <expression> $ <property>
--
prop_Insert5 x xs = ordered xs ==>
  collect (length xs) $
    ordered (insert x xs)
      where types = x :: Int

-- | Combining both collection of data values & classification
prop_Insert6 x xs = ordered xs ==>
  collect (length xs) $
    classify (ordered (x:xs)) "at-head"$
    classify (ordered (xs++[x])) "at-tail"$
    ordered (insert x xs)
      where types = x :: Int

-- | Handling recursive data types
-- | 
--
data Tree = Leaf Int | Branch Tree Tree

tree =
  oneof[
  liftM Leaf arbitrary,
  liftM2 Branch tree tree
       ]


