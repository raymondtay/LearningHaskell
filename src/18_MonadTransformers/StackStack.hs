
-- When Explicit Lifting is Necessary
--
-- A particular case where we must use "lift" is when we create a monad
-- transformer stack in which instances of the same typeclass appear at
-- multiple levels
--
import Control.Monad.Reader
import Control.Monad.State

type Foo    = StateT Int (State String)
type Foo' α = StateT Int (State String) α -- same as above! but it does prevent you from writing other type snyonyms.

-- if we try to use the "put" of the MonadState typeclass, the instance we will
-- get is that of "StateT Int", because it is at the top of the stack: 

outerPut :: Int -> Foo () -- replace Foo with Foo' and the meaning has not changed
outerPut = put

-- Example code:
-- runState ((runStateT (outerPut 3) 5)) "a"
-- (((),3),"a")
--
-- In this case, the only we can access the underlying State monad's "put" is
-- through use of "lift"
--
innerPut :: String -> Foo () -- replace Foo with Foo' and the meaning has not changed
innerPut = lift . put

-- Example code:
-- runState (runStateT (innerPut "a") 3) "h"
-- (((),3),"a")
--

-- If we want to apply a function to a doubly nested MTL, we have use explicit
-- lifting because it is necessary to do so. On one hand, it is a good style to
-- write wrapper funcions that do the lifting for us and on the other hand it
-- can look messy. Not sure if there is a way around it ...
--
type Bar = ReaderT Bool Foo
barPut :: String -> Bar ()
barPut = lift . lift . put



