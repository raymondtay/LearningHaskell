{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module UnderstandingGADTs where

import Prelude hiding (Maybe(..))
--
-- Source of motivation: https://en.wikibooks.org/wiki/Haskell/GADT
--
--
-- GADTs allow you to explicitly write down the types of the contructors
--


-- To understand why i need GADTs, i need to see where it failed without using
-- GADTs and how it can be remedied.
--

-- 1/ Imagine the following interpreter of expressions, the usual method of
--    writing evaluation functions is given below and it works great.
--    data Expr = I Int | Add Expr Expr | Mul Expr Expr
-- eval :: Expr -> Int
-- eval (I n) = n
-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Mul e1 e2) = eval e1 * eval e2

-- 2/ Now, if we expand the former expression to include other data types, we
--    start to discover that we had just opened a can of worms and it seems that
--    Haskell is unable to support what looks like simple expressions ... 
-- data Expr = I Int | B Bool | Add Expr Expr | Mul Expr Expr | Eq Expr Expr
-- This time, "Eval" can capture both Ints and Booleans so we want to express
-- that and i use the "Either" data type.
-- eval :: Expr -> Either Int Bool
-- eval (I n) = Left n
-- eval (B n) = Right n
-- eval (Add e1 e2) = undefined -- eval e1 + eval e2 -- what does it mean for bool + bool ? or int + bool or bool + int ? clearly, it does not make sense and it does not compute ... so we have to capture that as well
-- eval (Mul e1 e2) = undefined -- eval e1 * eval e2 -- same reasoning here
-- eval (Eq e1 e2) = undefined
-- 
-- 3/ Now that we need to cater to the following scenarios
--    3.1/ Support capture of booleans
--    3.2/ Support capture of integers
--    3.3/ Support nonsensical expressions
-- The following is how it might work
-- eval :: Expr -> Maybe (Either Bool Int)
-- eval (I n) = Just (Right n)
-- eval (B n) = Just (Left n)
-- eval (Add (I e1) (I e2)) = Just . Right $ (e1 + e2)
-- eval (Add (B e1) (B e2)) = Just . Left  $ (e1 && e2)
-- eval (Mul (I e1) (I e2)) = Just . Right $ (e1 * e2)
-- eval (Mul (B e1) (B e2)) = Just . Left  $ (e1 && e2)
-- eval (Add e1 e2) = (<*>) (fmap (liftA2 (+)) (eval e1)) (eval e2)
-- eval (Mul e1 e2) = (<*>) (fmap (liftA2 (*)) (eval e1)) (eval e2)
-- eval (Eq e1 e2)  = let a = fmap (fromLeft False) (eval e1)
--                        b = fmap (fromLeft False) (eval e2)
--                     in liftM Left ((<*>) (fmap (==) a) b)


-- Caveat: If i am NOT thinking correctly, the above expressions are prone to errors
-- and you can imagine the complexity grows as the DSL grows in complexity.

-- 
-- Phantom Types
--
-- The technique is to augment the "Expr" with a type variable
-- and basically we list the type signatures of all the constructors. In
-- particular, the marker type "a" is specialized to Int or Bool according to
-- the needs.
data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Eq a => Expr a -> Expr a -> Expr Bool

-- Now we can decide an evaluation function that takes advantage of the type
-- marker; convince yourself that the grammar and evaluation function captures
-- the Integers and Booleans.

eval :: Expr a -> a
eval (I n) = n
eval (B n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2

-- data Empty
-- data NonEmpty
-- data SafeList a b where
--   Nil :: SafeList a Empty
--   Cons :: a -> SafeList a b -> SafeList a NonEmpty
-- 
-- head :: SafeList a NonEmpty -> a
-- head (Cons h t) = h
-- 
-- headT :: SafeList a Empty -> a
-- headT Nil = undefined -- still does not make sense...

{-
 - When running the above you can see how it would work ...
 - 
 - *UnderstandingGADTs> UnderstandingGADTs.head (Cons 1 Nil)
 - 1
 - *UnderstandingGADTs> UnderstandingGADTs.head (Nil)
 - 
 - <interactive>:258:26: error:
 -     • Couldn't match type ‘Empty’ with ‘NonEmpty’
 -       Expected type: SafeList a NonEmpty
 -         Actual type: SafeList a Empty
 -     • In the first argument of ‘UnderstandingGADTs.head’, namely
 -         ‘(Nil)’
 -       In the expression: UnderstandingGADTs.head (Nil)
 -       In an equation for ‘it’: it = UnderstandingGADTs.head (Nil)
 -}


data NotSafe
data Safe

data MarkedList :: * -> * -> * where
  Nil :: MarkedList t NotSafe
  Cons :: a -> MarkedList a b -> MarkedList a c

-- safe extract the head of a "safe" list.
safeHead :: MarkedList a Safe -> a
safeHead (Cons x _) = x

-- safe extract the tail of a "safe" list.
safeTail :: MarkedList a Safe -> MarkedList a Safe
safeTail (Cons x (Cons x2 xs)) = Cons x2 xs

-- safely concatenating 2 "safe" lists.
safeConcat :: MarkedList a Safe -> MarkedList a Safe -> MarkedList a Safe
safeConcat l@(Cons x xs) r@(Cons y ys) = safeConcat (Cons y l) (safeTail r)


-- 
-- *UnderstandingGADTs> :t safeConcat (Cons 1 Nil) (Cons 2 Nil)
-- safeConcat (Cons 1 Nil) (Cons 2 Nil) :: Num a => MarkedList a Safe
-- 

