module MyRandom where


import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

-- System.Random contains pure random number generation functions. the
-- traditional downside of purity is that we have to get or create a random
-- nujber generator, and then ship it from the point we created it to the place
-- where it is neededl When we finally call it, it returns a new random number
-- generator - we are in pur code, remember so we cannot modify the state of
-- the existing generator.
-- If we forget about immutability and reuse the same generator within a
-- function, we get back exactly the same "random" number every time:
-- 

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)


type RAndomState a = State StdGen a

-- the type synonym is of course not necessary but it is handy. It saves a
-- little keyboarding and if we want to swap another random generator for
-- StdGen, it woudl reduce the number of type signatures we would need to
-- change.Generating a random value is now a matter of getching the current
-- generator using it then modifying the state to replace it with the new
-- generator:

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

