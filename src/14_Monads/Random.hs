import System.Random
import Control.Monad.State

-- A very simple state to increment '1' to its state and returns its previous
-- value
addOne :: State Int Int
addOne = do x <- get
            put (x+1)
            return x

-- combinator that "chains" 2 computations
addTwo :: State Int Int
addTwo = addOne >> addOne

addThree :: State Int Int 
addThree = addOne >> addOne >> addOne -- equivalently, you can write `addTwo >> addOne` and the operator can be replaced by '*>'

a = evalState addOne 0
b = runState addOne a

--
data CountedRandom = CountedRandom {
  crGen :: StdGen,
  crCount :: Int
                                   } deriving (Show)
-- The type alias `State` is defined as 
-- type State s = StateT s Data.Functor.Identity.Identity :: * -> *
--  	 Defined in ‘Control.Monad.Trans.State.Lazy’
-- which explains why you are reading it as this and not something like
-- \gen count -> State (\s -> ((), CountedRandom { crGen = gen, crCount = count }))
type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1}
  return val

getCount :: CRState Int
getCount = liftM crCount get

getCount' :: CRState Int -- liftM gives us the ability to apply a function (crCount) over the state.
getCount' = do st <- get
               return (crCount st)


putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }

