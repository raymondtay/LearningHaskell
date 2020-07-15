{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Foldable (forM_)
import Data.IORef

-- class Closet closet where
--   newClosetIO :: a -> IO (closet a)
--   getClosetIO :: closet a -> IO a
--   putClosetIO :: closet a -> a -> IO ()
-- 
-- instance Closet MVar where
--   newClosetIO = newMVar
--   getClosetIO = readMVar
--   putClosetIO closet a = modifyMVar_ closet (return . const a)

-- now we have the ability to write functions that are polymorphic over closets
data Present = Present { donorName :: String, presentName :: String } deriving Show

-- storeCloset :: Closet closet => [Present] -> IO (closet [Present])
-- storeCloset xs = do
--   closet <- newClosetIO []
--   forM_ xs $ \x -> do
--     old <- getClosetIO closet
--     putClosetIO closet (x : old)
--   return closet

-- There's a hidden problem here and that is we have committed to working in
-- the IO monad. What would be better is to be able to associate the type of
-- monad with the type of closet we are using - as knowing the closet tells us
-- the monad we have to work in.
--

class Closet closet where
  type ClosetMonad closet :: * -> *
  new :: a -> (ClosetMonad closet) (closet a)
  get :: closet a -> (ClosetMonad closet) a
  put :: closet a -> a -> (ClosetMonad closet) ()

instance Closet IORef where
  type ClosetMonad IORef = IO
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

instance Closet TVar where
  type ClosetMonad TVar = STM
  new = newTVar
  get = readTVar
  put ioref a = modifyTVar ioref (const a)

storeCloset :: (Closet closet, Monad (ClosetMonad closet)) => [Present] -> ClosetMonad closet (closet [Present])
storeCloset xs = do
  closet <- new []
  forM_ xs $ \x -> do
    old <- get closet
    put closet (x : old)
  return closet

main :: IO ()
main = do
  let presents = [ Present{ donorName = "Tim" , presentName = "Toaster" },
                   Present{ donorName = "Tim" , presentName = "Ironing Board" },
                   Present{ donorName = "John", presentName = "Vase" }]
  -- Here's how you can leverage the same'ol IO Monad
  -- ys  <- (storeCloset presents) :: IO (MVar [Present])
  -- _ys <- readMVar ys
  -- putStrLn . show $ _ys

  -- Here's how you can leverage IORef
  ys  <- (storeCloset presents) :: IO (IORef [Present])
  _ys <- readIORef ys
  putStrLn . show $ _ys

  -- Here's how you can leverage Software Transactional Memory i.e. STM
  ys  <- atomically $ ((storeCloset presents) :: STM (TVar [Present]))
  _ys <- readTVarIO ys
  putStrLn . show $ _ys
  return ()

