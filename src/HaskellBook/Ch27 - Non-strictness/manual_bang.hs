{-# LANGUAGE BangPatterns #-}

module ManualBang where

data DoesntForce = TisLazy Int String

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

-- note the exclamation marks again
--

data BangBang = SheShotMeDown !Int !String

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s

-- The main idea here is that in some cases, it is cheaper to just compute
-- something than to construct a thunk and then evaluate it later. This case is
-- particularly common in numerics code where you have a lot of Int and Double
-- values running around which are individually cheap to conjure. If the values
-- are both cheap to compute and small, then you may as well make them strict
-- unless you are trying to dance around bottoms. Types with underlying
-- primitive representations Int and Double most assuredly qualify as small.
--
-- A good rule to follow is lazy in the spine, strict in the leaves ! 
--
-- Sometimes a leak is not really a leak but temporarily excessive memory that
-- subsides because you made 1,000,000 tiny values into less-tiny thunks when
-- you could have just computed them as your algorithm progressed.
--
