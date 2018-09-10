import System.Random
import Control.Monad.State

data CountedRandom =
  CountedRandom {
    crGen :: StdGen, -- element of the state
    crCount :: Int } -- element of the state as well, so there's 2 elements of the state


type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return val

getCount :: CRState Int
getCount = crCount `liftM` get


-- Functors and monads are closely related. The terms are borrowed from a
-- branch of mathematics called category theory, but they did not make the
-- transition to Haskell completely unscathed.
--
-- Now that we know about the relationship between functors and monads, if we
-- look back at the list monad, we can see something interesting. Specifically,
-- take a look at the definition of (>>=) for lists
--
-- Recall that f has type a -> [a]. When we call map f xs, we get back a value
-- of tyep [[a]] which we have to "flatten" using concat. Consider what we
-- could do if Monad was a subclass of Functor. Since fmap for lists is defined
-- to be map, we could replace map with fmap in the definition  of (>>=).
--
--
--
