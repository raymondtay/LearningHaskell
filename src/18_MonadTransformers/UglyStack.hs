
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


data AppConfig = AppConfig { cfgMaxDepth :: Int } deriving (Show)

data AppState = AppState { stDeepestReached :: Int } deriving (Show)

-- What's this?
type App = ReaderT AppConfig (StateT AppState IO)

-- Before we go over there, let's break things down by understanding what does
-- each transformer do?
-- Let's start with the outermost "ReaderT" and the type signature looks like:
-- ReaderT :: (r -> m a) -> ReaderT r m a
-- "r" is the type of the input 
-- "m" is the monad in which this reader will execute under
-- "a" is the type of the result or returned value
-- 
-- let's understand it from this function
printContent :: ReaderT String IO ()
printContent = do
  content <- ask
  liftIO $ putStrLn ("The contents: " ++ show content)

g :: ReaderT String IO String
g = do
  answer <- ask
  liftIO $ putStrLn ("* The answer you gave me: " ++ show answer) -- escape hatch 
  return ("[" ++ answer ++ "]")

h :: ReaderT String IO String
h = do
  answer <- ask
  liftIO $ putStrLn ("! The answer you gave me: " ++ show answer) -- escape hatch 
  return (answer ++ "!")

-- Here's how i can chain readers and the nice thing about haskell syntax is
-- that the partial function application on the RHS of the bind-operator is subtle and
-- implicit
i = (runReaderT g "hi") >>= (runReaderT h)
j = do
  x <- i
  (runReaderT g x) *> (runReaderT h x)

-- Hopefully by the time we reach this point in time, it might be clearer
-- (might not be though :( ) how we might read the type of `App`
--
-- type App = ReaderT AppConfig (StateT AppState IO)
--
-- It might be easier to read it as 
-- "AppConfig"            is "r"
-- "(StateT AppState IO)" is "m"
-- and whatever happened to the mapping for "a" ??? We will see the answer in
-- the paragraph below. Before that, let's look at what "StateT AppState IO" is
-- saying based on the type signature - in the documentation, it reads 
--
-- newtype StateT s (m :: * -> *) a
--
-- where "s" is the state we want to maintain
--       "m" is the inner monad 
--       "a" is the result or return
-- and basically we map these definitions over to what we see - go do that
-- right now.
--
--
-- Where's the missing type parameter ?
--
-- You might have noticed that our type synonym does not have the usual type
-- parameter "a" that we associate with a monadic type:
--
-- type App2 a = ReaderT AppConfig (StateT AppState IO) a
--
-- Both App and App2 work fine in normal type signatures. The difference arises
-- when we try to construct another type from one of these. Say we want to add
-- another monad transformer to the stack: the compiler will allow WriterT
-- [String] App a but reject WriterT [String] App2 a.
--
-- The reason for this is that Haskell does not allow us to partially apply a
-- type synonym. The synonym App does not take a type parameter, so it doesn't
-- pose a problem. However, because App2 takes a type parameter, we must supply
-- some type for that parmaeter is we want to use App2 to create another type.
-- This restriction is limited to type synonyms. When we create a monad
-- transformer stack we usually wrap it with a newtype and as a result , weill
-- rarely run into this problem in practice.
--
runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runStateT (runReaderT k config) state

-- Now, let's explore about Writer Monads 
whatsMyName :: WriterT [String] (ReaderT String IO) ()
whatsMyName = do
  name <- ask
  tell ["You gave me: " ++ name]

getMyName = runReaderT (runWriterT whatsMyName)

type Log = [String]
type AppW = WriterT Log App ()

runAppW k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runWriterT (runStateT (runReaderT k config) state)

