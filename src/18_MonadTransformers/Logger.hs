
module Logger (
  Logger,
  Log,
  runLogger,
  record
  ) where

-- Our logger type is purely a type constructor; we don't expect the value
-- constructor that a user would need to create a value of this type. All they
-- can use Logger for is writing type signatures.
--

type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

-- When executing inside a Logger action, the user code calls record to record
-- something:
record :: String -> Logger ()
record s = Logger ((), [s])

newtype Logger a = Logger { execLogger :: (a, Log) }

-- The following are boilerplate code so we should be able to do away with them
    
instance Functor Logger where
  fmap f (Logger g) = Logger ((f . fst $ g), snd g)

instance Applicative Logger where
  pure a = Logger (a, [])
  (Logger f) <*> (Logger g) = Logger $ ((fst f $ fst g), snd g)

-- even though this definition compiles does not mean it is correct; why so?
-- The logger monad should combine the logs from both monads.
instance Monad Logger where
  return a = Logger (a, [])
  -- logger >>= f = f . fst $ execLogger logger INCORRECT
  logger >>= f = let (a, log) = execLogger logger
                     (a1,log1) = execLogger . f $ a
                 in Logger( a1, log ++ log1 ) 


