{-# LANGUAGE InstanceSigs #-}

module Logger( Logger, Log, runLogger, record ) where

type Log = [String] 

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = undefined

record :: String -> Logger ()
record s = Logger((), [s])

instance Functor Logger where
  fmap f (Logger v) = Logger $ (f (fst v), snd v)

instance Applicative Logger where
  pure :: a -> Logger a
  pure a = Logger(a, [])

  (<*>) (Logger fa) (Logger fb) = ((fst fa) <$> (Logger fb))

instance Monad Logger where
  return :: a -> Logger a
  return a = Logger(a, [])
  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  (>>=) (Logger a) f = let (a', w) = a
                           n       = f a'
                           (b, x)  = execLogger n
                       in Logger (b, w ++ x)


