
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
module EffectHandler where

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

instance Applicative (Nondet + Void) -- which allows the program to compile but what is it equivalent to ?? Compiler complains that of empty implementations ... 

knapsack2 :: Int -> [Int] -> Prog (Nondet + Void) [Int]
knapsack2 w vs | w < 0  = EffectHandler.fail
               | w == 0 = Return [] -- Fixed it by removing "EffectHandler.fail".
               | w > 0  = select2 vs >>= (\e -> knapsack2 (w - e) vs >>= (\es -> return (e:es)))

select2 :: [Int] -> Prog (Nondet + Void) Int
select2 = foldr (EffectHandler.||) EffectHandler.fail . map Return


{--
       State Effects
--}


-- Stateful operations are modelled with the assumption that there exists some
-- underlying state "s", which can be updated with the operation "put s", and
-- retrieved with "get". The corresponding syntax is as follows:
--
data State s cnt = Get' (s -> cnt) | Put' s cnt deriving Functor

pattern Get k <- (project -> Just (Get' k))
get :: (Applicative sig, State s ⊂ sig) => Prog sig s
get = inject (Get' return)

pattern Put s k <- (project -> Just (Put' s k))
put :: (Applicative sig, State s ⊂ sig) => s -> Prog sig ()
put s = inject (Put' s (return ()))

runState :: (Applicative sig) => s -> Prog (State s + sig) a -> Prog sig (s, a)
runState s (Return a) = return (s, a)
runState s (Get k)    = runState s (k s)
runState s (Put s' k) = runState s' k
runState s (Other op) = Op (fmap (runState s) op)

-- combines nondeterminism and state by providing the semantics for both
-- effects separately: we compose both handlers. How so? The first handler
-- tackles one effect in the initial program while the second handler tackles
-- the other in the residual program.
--

runLocal :: (Applicative (Nondet + sig), Applicative sig) => s -> Prog (State s + (Nondet + sig)) a -> Prog sig [(s, a)]
runLocal s = solutions . runState s

runGlobal :: (Applicative (State s + sig), Applicative sig) => s -> Prog (Nondet + (State s + sig)) a -> Prog sig (s, [a])
runGlobal s = runState s . solutions

-- It is interesting to me that these two composite semantics are not
-- equivalent; they differ in how the two effects interact. In runLocal, each
-- branch of the nondeterministic computation has its own local copy of the
-- state, while in runGlobal there is one state shared by all branches.
--
-- You can see this on the type level: that is "runLocal" returns a list of
-- solutions where each pair contains the local state and corresponding
-- solution; in "runGlobal" there's a single global state associated with a
-- list of solutions.
--

choices :: (Applicative sig, Nondet ⊂ sig, State Int ⊂ sig) => Prog sig a -> Prog sig a
choices (Return a) = return a
choices (Fail)     = EffectHandler.fail
choices (p :|| q)  = incr >> (choices p EffectHandler.|| choices q)
choices (Op op)    = Op (fmap choices op)

incr :: (Applicative sig, State Int ⊂ sig) => Prog sig ()
incr = get >>= put . (succ :: Int -> Int)

-- Given this, how would the knapsack problem look like now?
--
knapsack3 :: Int -> [Int] -> Prog (Nondet + Void) [Int]
knapsack3 w vs | w < 0  = EffectHandler.fail
               | w == 0 = Return [] -- Fixed it by removing "EffectHandler.fail".
               | w > 0  = select3 vs >>= (\e -> knapsack3 (w - e) vs >>= (\es -> return (e:es)))

select3 :: [Int] -> Prog (Nondet + Void) Int
select3 = foldr (EffectHandler.||) EffectHandler.fail . map Return

