module Chapter04 where

-- Polymorphism in Haskell means being able to write code in terms
-- of values which may be one of several, or any, type. 
-- Polymoprhism in Haskell is either parameteric or constrained. The identity function `id` i an example of a parameterically
-- polymorphic function:
-- id :: a -> a
-- id x = x
--

-- Fill in the definition of the following function
-- using `fst` and `snd`.
f :: (a,b) -> (c, d) -> ((b,d), (a, c))
f x y = (,) ((,) (snd x) (snd y)) ((,) (fst x) (fst y))

