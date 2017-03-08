module Chapter_18 where

import Control.Monad

-- 
-- The exercise on page 760 deserves an explanation. 
-- I used 'f' and 'm' to represent (a -> m b) and (m a) respectively
-- Next thing is to take notice of the fact that we can fmap 'f' and 'm'
-- but that creates a nested monadic structure (if there is such a term) 
-- and so i use the `join` in `Control.Monad` to flatten it.
--
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

