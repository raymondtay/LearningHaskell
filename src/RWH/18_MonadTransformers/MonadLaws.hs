
import Prelude hiding ((>>=) , return)

  
class Functor m => AltMonad m where
  join :: m (m a) -> m a
  join x = x >>= id
  return :: a -> m a

(>>=) :: AltMonad m => m a -> (a -> m b) -> m b
xs >>= f = join (fmap f xs)

-- Left identity for (>>=) 
--  
-- return x >>= f == f x
-- Another way to phrase this is that ther eis no reason to use return to wrap
-- up a pure value if all you are going to do is unwrap it again with (>>=) .
-- It is actually a common style error among programmers new to monads to wrap
-- a value with return, and then unwrap it with (>>=)  a few lines later in the
-- same function.


