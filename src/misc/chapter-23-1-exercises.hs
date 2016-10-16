{-# LANGUAGE InstanceSigs #-}

module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

import System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving(Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
              1 -> DieOne
              2 -> DieTwo
              3 -> DieThree
              4 -> DieFour
              5 -> DieFive
              6 -> DieSix
              x -> error $ "intToDie got non of 1-6 integers: " ++ show x

-- 
-- This code isn't optimal but it does work. it will produce the 
-- same results every time, because it is free of effects, but you can 
-- make it produce a new result on a new dice roll if you modify the 
-- start value.
--
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s ) <- randomR (1, 6)
  return (intToDie n, s)

-- 
-- The interesting bits here is related to
--
-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- 
-- where 
--
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- here's an example of how to run this:
--
-- *RandomExample System.Random Control.Applicative> evalState rollDieThreeTimes' (mkStdGen 0)
-- (DieSix,DieSix,DieFour)
-- *RandomExample System.Random Control.Applicative> evalState rollDieThreeTimes' (mkStdGen 4)
-- (DieSix,DieThree,DieThree)
-- *RandomExample System.Random Control.Applicative> evalState rollDieThreeTimes' (mkStdGen 6)
-- (DieSix,DieOne,DieSix)
--

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- here's how to use `infiniteDie`:
--
-- *RandomExample System.Random Control.Applicative> take 5 $ evalState infiniteDie (mkStdGen 0)
-- [DieSix,DieSix,DieSix,DieSix,DieSix]
--
--


nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- here's how to use `nDie`:
--
-- *RandomExample System.Random Control.Applicative> evalState (nDie 4) (mkStdGen 0)
-- [DieSix,DieSix,DieFour,DieOne]
-- *RandomExample System.Random Control.Applicative> evalState (nDie 9) (mkStdGen 0)
-- [DieSix,DieSix,DieFour,DieOne,DieFive,DieTwo,DieFour,DieTwo,DieTwo]
--


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise = 
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

--
-- Refactor rollsToGetTwenty into having the limit
-- be a function argument.
--
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN = undefined


--
-- Despite a few months ago, this is still tricky for me.
-- Sigh :( - just keep trying but in any case, again you (i mean "me")
-- need to remember that `Moi s a` is another way of writing `s -> (a, s)`
--
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s1) = g s in (f a, s1) 

-- 
-- Writing or exposing the type signature of the various
-- functions i'm trying to provide an explanation of is
-- useful and helpful in helping me work through as a beginner
--
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = 
    Moi $ \s ->
    let
      (f', _) = f s -- pull the `a -> b` out from f
      (a', _) = g s -- pull the value from `s -> (a, s)` of g
    in (f' a', s) -- we basically ignore the derived states from the application of 's' to 'f' and 'g'

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let
      (a, _) = f s -- alternatively, we can write `a = fst $ f s`
      (b, _) = runMoi (g a) $ s -- alternatively, we can write `b = fst $ runMoi (g a) $ s`
    in (b, s)


