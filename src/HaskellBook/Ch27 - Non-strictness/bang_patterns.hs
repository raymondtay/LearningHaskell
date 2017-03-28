{-# LANGUAGE BangPatterns #-}

-- Bang-patterns
-- sometimes we want to evaluate an argument to a function whether we use it or
-- not. We can do this via `seq`:
--

module ManualBang where

doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

-- we can also achieve the ↑ effect 

banging :: Bool -> Int
banging !b = 1

-- Bang patterns in data
--
-- when we evaluate the outer data constructor of a data type, at times we
-- would also like to evaluate the contents to weak head normal form just like
-- with functions.
--
-- a simple way to see the difference between strict and non-strict constructor
-- arguments is how they behave when they are undefined. 
--
data Foo = Foo Int !Int

first (Foo x _) = x
second (Foo _ y) = y

-- since the non-strict argument isn't evaluated by `second`, passing in
-- `undefined` doesn't cause a problem
--
