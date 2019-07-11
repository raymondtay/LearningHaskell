
import Control.Monad.State
import Control.Monad.Reader

type Foo = StateT Int (State String)

-- Its a little mind boggling how this works, the book's example clearly
-- demonstrates the need to
outerPut :: Int -> Foo ()
outerPut = put

-- in this case, the only way we can access the underlying State monad's put is
-- through the use of "lift", as given below.
innerPut :: String -> Foo ()
innerPut = lift . put

-- The approach to understanding this is to develop small examples and
-- convince yourself

-- Sometimes, we need to access a monad more than one level down the stack, in
-- which case we must compose calls to lift. Each composed use of lift gives us
-- access to one deeper level:
--

type Bar = ReaderT Bool Foo  -- this is really `ReaderT Bool (StateT Int (State String))`

barPut :: String -> Bar ()
barPut = lift . lift . put

-- The point of demonstrate is really to galvanize the learnings on applying
-- the lifting technique to access deeply nested monads in your code.
demonstrate =
  let o = outerPut 42
      oi = innerPut "wassup dude?"
      bp = barPut "WHAT_IS_THIS"
   in (runState (runStateT o  3333) "donkeykong" ,
       runState (runStateT oi 3333) "kongdonkey",
       runState (runStateT (runReaderT bp True) 3333) "donkey_kong") where
        validate result expected = result == expected


