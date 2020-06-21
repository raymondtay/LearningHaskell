{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Foldable (forM_)
import Data.IORef
import Control.Monad

--
-- Article of inspiration: http://wiki.haskell.org/Functional_dependencies
--

-- Without fundeps, the two (2) instances would be permissible; if that was
-- intended that would not be a problem. Question is whether that was intended?
-- 
-- If that was not intended, then fundeps would be incredibly useful here ;)


--
-- class Extract container elem where
--   extract :: container -> elem
--
-- The above definition is the same as the one following ... 
--
-- This definition of "container" is too general and GHC does not know how to
-- decompose "container" into it constituents else it would have been obvious.
--
class Extract elem container | container -> elem where
  extract :: container -> elem

instance Extract b (a, b) where
  extract (x, y) = y

instance Extract a (a, b) where
  extract (x, _) = x

--
-- The canonical use case of leveraging fundeps to define "permissible"
-- relationships.
--
class D a b | a -> b where
  don :: a -> b

instance D Bool Char where
  don True = 't'
  don False = 'f'

instance D (a, b) b where
  don (_, y) = y

instance D (a, b) a where -- this is another example of why "a" is general for "D"
  don (x, y) = x

-- Uncomment the following to witness the fundeps at play
-- instance D Bool Int where
--   don True = 1
--   don False = 0

class Store store m | store -> m where
 new :: a -> m (store a)
 get :: store a -> m a
 put :: store a -> a -> m ()

instance Store IORef IO where
  new = newIORef
  get = readIORef
  put ref a = modifyIORef ref (const a)

type Present = String
storePresents :: (Store store m, Monad m) => [Present] -> m (store [Present])
storePresents xs = do
  store <- new []
  forM_ xs $ \x -> do
    old <- get store
    put store (x : old)
  return store

ex :: [Present] -> IO [Present]
ex ps = do
  store <- storePresents ps
  get (store :: IORef [Present])

class Container e c | c -> e where
  empty  :: c
  insert :: e -> c -> c
  member :: e -> c -> Bool
  toList :: c -> [e]

instance Eq e => Container e [e] where
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs) 
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l

