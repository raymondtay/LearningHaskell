
-- When Explicit Lifting is Necessary
--
-- A particular case where we must use "lift" is when we create a monad
-- transformer stack in which instances of the same typeclass appear at
-- multiple levels
--
import Control.Monad.State

type Foo = StateT Int (State String)

-- if we try to use the "put" of the MonadState typeclass, the instance we will
-- get is that of "StateT Int", because it is at the top of the stack: 

outerPut :: Int -> Foo ()
outerPut = put

-- Example code:
-- runState ((runStateT (outerPut 3) 5)) "a"
-- (((),3),"a")
--
-- In this case, the only we can access the underlying State monad's "put" is
-- through use of "lift"
--
innerPut :: String -> Foo ()
innerPut = lift . put

-- Example code:
-- runState (runStateT (innerPut "a") 3) "h"
-- (((),3),"a")
--
-- 
