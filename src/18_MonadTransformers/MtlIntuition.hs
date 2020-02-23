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

innerMod :: (Int -> Int) -> Barr ()
innerMod = lift . modify

-- Something interesting happened here during my experimentation. The definition of this function
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

-- Qn: Given a user defined state function (e.g. addOne), how does one use it in
--     a function (e.g. test4) where we modify it?
--
test4 :: (Int -> Int) -> Barr Int
test4 f = do
  b <- ask
  case b of
      True -> lift $ withState f addOne
      False -> state (\s -> (3, s))

-- *> runStateT (runReaderT (test4 (+1)) False) 21
-- Identity (3,21)
-- *> runStateT (runReaderT (test4 (+1)) True) 21
-- Identity (22,23)

-- Qn: Given a user defined state function (e.g. addOne), how does one use it in
--     a function (e.g. test4) where we modify it and the twist here is that i
--     wished for the result to be suppressed i.e. () and i've found 1 way to
--     solve this by application of "mapState".
--
test5 :: (Int -> Int) -> Barr ()
test5 f = do
  b <- ask
  case b of
      True -> lift $ withState f $ mapState ((\(a, s) -> ((), s)) :: (Int, Int) -> ((), Int)) addOne
      False -> state (\s -> ((), s))


