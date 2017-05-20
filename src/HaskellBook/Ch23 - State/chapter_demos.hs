{-# LANGUAGE InstanceSigs #-}

module Chapter23_demos where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show) 

intToDie :: Int -> Die
intToDie n = case n of
                 1 -> DieOne
                 2 -> DieTwo
                 3 -> DieThree
                 4 -> DieFour
                 5 -> DieFive
                 6 -> DieSix
                 x -> error $ "intToDie got non 1-6 integers" ++ show x

-- Don't use 'error' outside of experiments like this, or in cases where 
-- the branch you are ignoring is provably impossible. we do not use the word
-- provably here lightly.
--

rollDie :: State StdGen Die
rollDie = state $ do 
  (n, s) <- randomR (1, 6)
  return (intToDie n ,s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

