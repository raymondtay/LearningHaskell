
-- Learning the Control.Exception & Control.Concurrent libraries
--
import Control.Concurrent
import Control.Exception
import System.IO          -- for the stdin, stderr etc
import System.IO.Error    -- for the userError, mkIOError etc
import Control.Monad.Except
import Control.Monad.Trans.Except

simple = do
  tid <- forkIO $ putStrLn "yes i'm in a thread"
  putStrLn $ "\n\tthread: " ++ show tid ++ " executed\n"


data RidiculousException = RidiculousException deriving (Show)

instance Exception RidiculousException -- now a member of the [[Exception]] typeclass.

willThrow :: a -- naughty function which vomits every.single.time.
willThrow = throw RidiculousException

exceptionHandler :: Exception e => e -> IO ()
exceptionHandler = \e -> do
  putStrLn ("Exception caught: " ++ show (toException e))

wasItCaught = catch willThrow (exceptionHandler :: RidiculousException -> IO ())

-- whenever you run this, you are likely to see the output of "+" interleaved
-- with "..." and sometimes GHCi would vomit the exception onto the
-- console.
whatDoesThrowToDo = do
  tid <- forkIO $ putStrLn "+"
  putStrLn "..."
  throwTo tid RidiculousException

{-| Some examples from System.IO.Error -}

data MyIOError = SpecialIOError deriving (Show)
instance Exception MyIOError

-- IOError is a type alias of IOException and turns out that there are 2 ways
-- to create an IOError : (a) userError (b) mkIOError and below is an example
-- of how to use `userError`.
simpleCatchIOError = 
  catchIOError (do { throw (userError "Error lah") }) (\ioe -> do { return 55 })

-- | The canonical way is roughly broken down to:
-- | (a) Define the error types
-- | (b) Define the business logic and use the appropriate exception functions
--       e.g. throwError
-- | (c) define instances of `Show` for your error types if you do decide to
--       output useful information w/o belaboring the functions
--
-- type to represent length calculation error
data LengthError = EmptyString -- entered string was empty
                 | StringTooLong Int -- a string is longer than 5 chars
                 | OtherError String -- other error, the payload is a description


type LengthMonad = Either LengthError

main1 = do
  putStrLn "enter a string.."
  s <- getLine
  reportResult (calculateLength s)

calculateLength :: String -> LengthMonad Int -- i.e. Left(<error msg>) or Right(<length>)
calculateLength [] = throwError EmptyString
calculateLength s | len > 5 = throwError (StringTooLong len)
  | otherwise = return len
  where len = length s

reportResult :: LengthMonad Int -> IO ()
reportResult (Right len) = putStrLn ("The length of the string: " ++ (show len))
reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))

instance Show LengthError where
  show EmptyString = "The string was empty!"
  show (StringTooLong len) = "The string was too long by " ++ (show (len - 5)) ++ " characters!"
  show (OtherError msg) = msg

-- Next is to see how to use ExceptT mtl to rework the previous function and
-- below is an example of this attempt. What is interesting is that it no
-- longers work
type LengthMonadT = ExceptT String IO -- remember that its ExceptT e m a where 'm' ∈ Monad and its really m (Either e a)

calculateLength2 :: LengthMonadT Int
calculateLength2 = do
  liftIO (putStrLn "Please enter a non-empty string:")
  s <- liftIO getLine
  if null s then throwError "The string was empty!" else return (length s)
 
reportResult2 :: Either String Int -> IO ()
reportResult2 (Right len) = putStrLn ("The length of the string: " ++ (show len))
reportResult2 (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))

main2 = do
  -- runExceptT removes the ExceptT wrapper
  r <- runExceptT calculateLength2
  reportResult2 r

calculateL :: String -> LengthMonadT Int -- i.e. Left(<error msg>) or Right(<length>)
calculateL [] = throwE "empty string!"
calculateL s | len > 5 = throwE "string is too long!"
  | otherwise = return len
  where len = length s

main3 = do
  putStrLn "enter a string.."
  s <- getLine
  r <- runExceptT (calculateL s)
  reportResult2 r

-- Its kind of a waste to not leverage the Exception data types defined, so i
-- looked into how i can fix this problem. Turns out i can do something about
-- it and this style appeals to me more and the primary reason is that i can
-- re-use the data types and the typeclass machinery which brings a big smile
-- to my face ☺

type LengthMonadTT = ExceptT LengthError IO

calculateLL :: String -> LengthMonadTT Int
calculateLL [] = throwE EmptyString
calculateLL s | len > 5 = throwE (StringTooLong len)
  | otherwise = return len
  where len = length s

main4 = do
  putStrLn "enter a string.."
  s <- getLine
  r <- runExceptT (calculateLL s)
  reportResult r


main = main4

