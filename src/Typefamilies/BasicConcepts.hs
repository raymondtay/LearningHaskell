{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

--
-- Source of motivation for this exercise : https://ocharles.org.uk/posts/2014-12-12-type-families.html
--
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Foldable (forM_)
import Data.IORef
import Control.Monad.IO.Class

class IOStore store where
  newIO :: a -> IO (store a)
  getIO :: store a -> IO a
  putIO :: store a -> a -> IO ()

instance IOStore MVar where
  newIO = newMVar
  getIO = readMVar
  putIO store a = modifyMVar_ store (return . const a)

-- now we have the ability to write functions that are polymorphic over stores
type Present = String

storePresentsIO :: IOStore store => [Present] -> IO (store [Present])
storePresentsIO xs = do
  store <- newIO []
  forM_ xs $ \x -> do
    old <- getIO store
    putIO store (x : old)
  return store

-- There's a hidden problem here and that is we have committed to working in
-- the IO monad. What would be better is to be able to associate the type of
-- monad with the type of store we are using - as knowing the store tells us
-- the monad we have to work in.
--
-- To use type families, we use the `type` keyword within the `class` definition,
-- and specify the kind of the type
--

class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)
  get :: store a -> (StoreMonad store) a
  put :: store a -> a -> (StoreMonad store) ()

instance Store IORef where
  type StoreMonad IORef = IO
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

instance Store TVar where
  type StoreMonad TVar = STM
  new = newTVar
  get = readTVar
  put ioref a = modifyTVar ioref (const a)

storePresents :: (Store store, Monad (StoreMonad store)) => [Present] -> StoreMonad store (store [Present])
storePresents xs = do
  store <- new []
  forM_ xs $ \x -> do
    old <- get store
    put store (x : old)
  return store

main :: IO ()
main = do
  let presentsFromTim = ["toaster", "picture frame", "toilet paper"]
      presentsFromJon = ["vase"]
  ys  <- (storePresents presentsFromJon) :: IO (IORef [Present])
  _ys <- readIORef ys
  zs  <- (storePresents $ presentsFromTim ++ _ys) :: IO (IORef [Present])
  _zs <- readIORef zs
  putStrLn . show $ _zs
  return ()

-- The benefits of type families allows us to not only extend the existing type
-- class functionality, but also we can leverage it to make sure we are not
-- always stuck in the IO monad and instead a Monad of our desire, or any Monad
-- for which the instance can cater to.
--
