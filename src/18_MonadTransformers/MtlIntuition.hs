-- The intention of this simple exercise is to develop an intuition for mtl
-- and how to use it in general. The approach i've taken here is to approach it
-- from a top-down i.e. by studying the "mtl" library and then construct small
-- examples so that i can understand them completely.
--
-- The second approach is to understand, from the root, by understanding how it
-- all began. This approach is slightly harder to comprehend as it examines the
-- basic intuition and to reach maximum comprehension - it would wise if you
-- gained some proficiency in the language a priori.
-- 
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

-- Step-1/ 
-- Start by understanding the mechanics of the fundamental Monad
type Bar = Reader Bool Int

test1 :: Bar
test1 = do
  a <- ask
  case a of
    True -> return 1
    False -> return (-1)

-- Step-2/
-- Leverage the previous step by embedding a monad in the MTL
-- In this example, "State" is the inner Monad and ReaderT is the "outer"
-- Monad.
type BarT = ReaderT Bool (State String)

test2 :: BarT Int
test2 = do
  b <- ask
  case b of 
    True -> state (\s -> (1, s))
    False -> state (\_ -> ((-1), ""))

-- when i test drove the above "undefined" expr via 
-- *> runStateT
-- (runReaderT test2 True) "hallo"
-- Identity (1,"hallo")
-- *> runStateT
-- (runReaderT test2 False) "hallo"
-- Identity (-1,"")

-- Now, let's experiment what it feels like to have the state function defined outside, rationale for
-- taking this approach is because this is typical of programmers i.e. define some function somewhere
-- and leverage haskell to stitch it into something bigger.

-- Step-3/
-- finally, leverage your proficiency by imagining a pre-defined function (e.g.
-- "addOne") and see how you can fit into the situation.
--
addOne :: State Int Int
addOne = do
  x <- get
  put (x+1)
  return x

type Barr = ReaderT Bool (State Int)

test3 :: Barr Int
test3 = do
  b <- ask
  case b of 
    True -> lift addOne -- key insight here is to leverage "lift"
    False -> state (\_ -> ((-1), (-1)))

-- When i experimented this move, here is what i observed:
-- *> runStateT (runReaderT test3 False) 4
-- Identity (-1,-1)
-- *> runStateT (runReaderT test3 True) 4
-- Identity (4,5)

innerAddTwo :: (Int -> Int) -> Barr ()
innerAddTwo = lift . modify

-- Something interesting happened here. The definition of this function
-- "innerAddTwo" forced the types to be consistent with its signature. Do you
-- see it?
-- Its easy to confirm this observation when you noticed that the
-- interpretation of the boolean is no longer that in "test3"...interesting
-- isn't it?
--
-- *> runStateT (runReaderT (innerAddTwo (\s -> s+1)) False) 4
-- Identity ((),5)
-- *> runStateT (runReaderT (innerAddTwo (\s -> s+1)) True) 4
-- Identity ((),5)
--
--

test4 :: (Int -> Int) -> Barr ()
test4 f = do
  b <- ask
  case b of
      -- True -> (lift . modify) f 
      True -> lift (modify f) . lift addOne
      -- False -> state (\_ -> (42, ()))
-- *> runStateT (runReaderT (test4 id) True) 44
-- Identity ((),44)
-- *> runStateT (runReaderT (test4 id) False) 44
-- Identity *** Exception: MtlIntuition.hs:(96,3)-(97,31): Non-exhaustive patterns in case

