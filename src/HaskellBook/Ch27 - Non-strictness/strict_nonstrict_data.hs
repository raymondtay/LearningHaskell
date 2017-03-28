{-# LANGUAGE Strict #-}
{-# LANGUAGE BangPatterns #-}

module StrictTest where

blah x = 1

main = print (blah undefined)

-- So, the Strict and StrictData pragmas are a means to avoiding noise when
-- everything or almost everything in a module is supposed to be strict. You
-- can use the tilde for irrefutable patterns to recover laziness on a case by
-- case basis.
--

willForce x = 1
willNotForce ~x = 1

-- our List and sTake is lazy here. 
--
data List a = Nil | Cons a (List a) deriving Show

sTake :: Int -> List a -> List a
sTake n _ 
  | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x xs) = (Cons x (sTake (n-1) xs))

twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls

-- Noting the placement of the exclamation marks denoting strictness, run the
-- entire code base in GHCi and see what happens.
--
data List' a = Nil'| Cons' !a (List' a) deriving Show

sTake' :: Int -> List' a -> List' a
sTake' n _ 
  | n <= 0 = Nil'
sTake' n Nil' = Nil'
sTake' n (Cons' x xs) = (Cons' x (sTake' (n-1) xs))

twoEls' = Cons' 1 (Cons' undefined Nil')
oneEl' = sTake' 1 twoEls'

data List'' a = Nil'' | Cons'' !a (List'' a) deriving Show

-- the strict evaluation is right now applied to "xs" and it would evident when
-- you play around 
sTake'' :: Int -> List'' a -> List'' a
sTake'' n _ | n <= 0 = Nil''
sTake'' n Nil'' = Nil''
sTake'' n (Cons'' x !xs) = (Cons'' x (sTake'' (n-1) xs))

twoEls'' = Cons'' 1 (Cons'' undefined Nil'')
oneEl'' = sTake'' 1 twoEls''

