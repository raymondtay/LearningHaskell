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

