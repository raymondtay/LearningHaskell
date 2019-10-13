
{-# LANGUAGE DeriveFunctor        ,
             InstanceSigs         ,
             TypeOperators        ,
             MultiParamTypeClasses,
             FlexibleInstances    ,
             FlexibleContexts     ,
             PatternSynonyms      ,
             EmptyDataDecls       ,
             DeriveAnyClass       ,
             ViewPatterns           #-}

-- Notes: In order to get this program to compile, i leveraged the extensions:
-- (a) EmptyDataDecls which is enabled by default in GHC 8.6.5 but i feel
-- better to make it explicit as i'm still learning these advanced features.
-- (b) DeriveAnyClass to auto-derive Applicative for empty data type 'data Void cnt'
--     Note: Be aware of what you are doing !!! because there're empty
--     implementations which means a runtime error will be thrown if you are
--     not careful !
--
module Prog where

import Control.Monad      (join, liftM2)

-- We can generalize away from backtrackable computations by defining a
-- datatype that is parametric in the signature of the syntax.
-- We factor syntax for programs into the "Return" contructor and a constructor
-- "Op" that allows us to incorporate operations of interest from some signature
-- "sig".
data Prog sig a = Return a | Op (sig (Prog sig a)) deriving Functor

data Nondet cnt = Fail' | cnt :| cnt deriving Functor

data Void cnt deriving (Functor, Applicative)

instance (Applicative sig) => Applicative (Prog sig) where
  pure :: a -> Prog sig a
  pure = Return
  
  (<*>) :: Prog sig (a -> b) -> Prog sig a -> Prog sig b
  (<*>) (Return f) (Return a) = Return (f a)
  (<*>) (Op op) b = Op $ fmap (\innerOp -> innerOp <*> b) op

instance (Applicative sig) => Monad (Prog sig) where
  return = Return

  (Return v) >>= prog = prog v
  Op op >>= prog = Op(fmap (>>= prog) op)
  



-- Generally speaking, we will be dealing with several different effects that
-- work together in a single program, and so we need a means of flexibly
-- composing signatures, where each signature captures syntax that encodes a
-- particular effect.
--
data (sig1 + sig2) cnt = Inl (sig1 cnt) | Inr (sig2 cnt) deriving Functor

class (Functor sub, Functor sup) => sub ⊂ sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor sig => sig ⊂ sig where
  inj = id
  prj = Just

instance (Functor sig1, Functor sig2) => sig1 ⊂ (sig1 + sig2) where
  inj = Inl
  prj (Inl fa) = Just fa
  prj _ = Nothing

-- instance (Functor sig1, sig ⊂ sig2) => sig ⊂ (sig1 + sig2) where
--   inj = Inr . inj
--   prj (Inr ga) = prj ga
--   prj _ = Nothing

inject :: (sub ⊂ sup) => sub (Prog sup a ) -> Prog sup a
inject = Op . inj

project :: (sub ⊂ sup) => Prog sup a -> Maybe (sub (Prog sup a))
project (Op s) = prj s
project _ = Nothing

pattern Fail <- (project -> Just Fail') 
fail :: (Nondet ⊂ sig) => Prog sig a
fail = inject Fail'

pattern p :|| q <- (project -> Just (p :| q))
(||) :: (Nondet ⊂ sig) => Prog sig a -> Prog sig a -> Prog sig a
p || q = inject (p :| q)

run :: Prog Void a -> a
run (Return x) = x -- this is typically the last to run, since it extracts a final value from a program.

pattern Other s = Op (Inr s)

solutions :: (Applicative sig) => Prog (Nondet + sig) a -> Prog sig [a]
solutions (Return a) = return [a]
solutions (Fail)     = return []
solutions (p :|| q)   = liftM2 (++) (solutions p) (solutions q)
solutions (Other op) = Op (fmap solutions op)

allsolutions2 :: Prog (Nondet + Void) a -> [a]
allsolutions2 = run . solutions

instance Applicative (Nondet + Void) -- likely to be incorrect

-- Program compiles but doesn't work as expected.
knapsack2 :: Int -> [Int] -> Prog (Nondet + Void) [Int]
knapsack2 w vs | w < 0  = Prog.fail
               | w == 0 = Prog.fail
               | w > 0  = select2 vs >>= (\e -> knapsack2 (w - e) vs >>= (\es -> return (e:es)))

select2 :: [Int] -> Prog (Nondet + Void) Int
select2 = foldr (Prog.||) Prog.fail . map Return


