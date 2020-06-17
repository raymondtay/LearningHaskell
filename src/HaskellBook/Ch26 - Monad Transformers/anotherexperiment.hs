
import Control.Monad.State

-- one case in which we must use lift is when we create a monad transformer
-- stack in which instances of the same typeclass appear at multiple levels.
--
type Foo = StateT Int (State String)

outerPut :: Int -> Foo ()
outerPut = put

