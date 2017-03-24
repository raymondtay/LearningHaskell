{-# LANGUAGE InstanceSigs #-}

module Chapter_27 where

-- Technically, Haskell is only obligated to be non-strict, not lazy. A truly
-- lazy language memoizes, or holds in memory, the results of all the functions
-- it does evaluate, and outside of toy programs, this tends to use
-- unacceptably large amounts of memory.
--
-- Implementations of Haskell, such as GHC Haskell, are only obligated to be
-- non-strict s.t. they have the same behavior w.r.t bottom; they are not
-- required to take a particular approach to how the program executes or how
-- efficiently it does so.
--
--

-- Prelude> :t fst
-- fst :: (a, b) -> a
-- Prelude> fst (1, undefined)
-- 1
-- Prelude> snd (1, undefined)
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
  -- error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  -- undefined, called at <interactive>:3:9 in interactive:Ghci2
-- Prelude>
-- 
-- Prelude> snd (undefined, 2)
-- 2

possiblyKaboom = \f -> f fst snd (42, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

-- let's dissect the function application `possiblyKaboom true` which gives 42, actually.
-- (\f -> f fst snd (42, undefined)) (\a -> (\b -> a))
-- (\a -> (\b -> a)) fst snd (42, undefined)
-- (\b -> fst) snd (42, undefined)
-- fst (42, undefined) 
-- 42 !!!
--
-- Another example of the function, is written here which does exactly the same
-- thing as 'possiblyKaboom'. A little writeup is in order here. The bottom is
-- inside a tuple, and the tuple is bound inside of a lambda that cases on a
-- boolean value and returns either the first or second of element of the
-- tuple. Since we start evaluating from the outside, as long as this function
-- is only ever applied to True, that bottom will never cause a problem.
-- However, at the risk of starting the obvious, we do not encourage you to
-- write programs with bottoms lying around willy-nilly.
--
-- When we say evaluation works outside in, we are talking about evaluatig a
-- series of nested expressions, and not only are we starting from the outside
-- and working in, but we are also only evaluating some of the expressions some
-- of the time. In haskell, we evaluate expressions when we need them rather
-- than when they are first referred to or constructed. This is one of the ways
-- in which non-strictness makes haskell programs expressive - we can refer to
-- values before we have done the work to create them.
-- 
possiblyKaboom' b =
  case b of
      True -> fst tup
      False -> snd tup
      where tup = (42, undefined)
            
