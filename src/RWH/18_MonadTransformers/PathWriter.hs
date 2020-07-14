
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

pathsWriter :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int,Int)] -> Int -> Int -> [Writer [Int] ()]
pathsWriter' edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- pathsWriter' edges e_end end
                   return $ do tell [start]
                               subpath
   in if start == end then tell [start] : e_paths else e_paths

pathsWriterT' :: [(Int,Int)] -> Int -> Int -> WriterT [Int] [] () 
pathsWriterT' edges start end =
  let e_paths = do (e_start, e_end) <- lift edges
                   guard $ e_start == start
                   tell [start]
                   pathsWriterT' edges e_end end
   in if start == end then tell [start] `mplus` e_paths else e_paths

pathsWriterT :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)
-- load this program into ghci
-- *Main> main
-- [[2013,501,2558],[2013,1004,2558]] -- this is from the non-MTL version
-- [[2013,501,2558],[2013,1004,2558]] -- this is from the MTL version
-- (43,"42") -- this is from the Reader + Writer combo
--
main :: IO ()
main = do
  let graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]
  putStrLn . show $ pathsWriter graph1 2013 2558
  putStrLn . show $ pathsWriterT graph1 2013 2558
  putStrLn . show $ runWriter (runReaderT readerWriterExample 42)


readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         lift . tell $ show x
                         return $ x + 1

-- Two States At a Time
-- Write a function that compute the factorial of a number. But instead of the
-- usual implementation , use one based on two states: one for keeping a
-- decreasing counter and another one for keeping the factorial. One approach
-- is to keep the state as a tuple, but for this exercise i want you to
-- implement  the state using a StateT Integer (State Integer) monad. Thus you
-- must use lift to acces one of the internal states. The final code to run the
-- monad computation should be similar to similar to `execStateT (execState factorial x) 1`
--

-- *Main> execState (runStateT factorial $ 10) 1
-- 3628800
-- *Main> execState (runStateT factorial $ 4) 1
-- 24
factorial :: StateT Integer (State Integer) ()
factorial = do
  s <- get
  if s == 0 then lift . put $ 1 else put (s-1) ; lift . put $ foldl (*) 1 [1..s]


-- leverage directly `mfilter` instead of `msum`;
find_ :: (a -> Bool) -> [a] -> Maybe a
find_ p xs = case mfilter p xs of
  [] -> Nothing
  [x] -> Just x

find2_ :: (a -> Bool) -> [a] -> Maybe a
find2_ p xs = msum $ fmap (\x -> case (p x) of True -> Just x; False -> Nothing) xs

-- Every monad admits a function called liftM of type Monad m => (a -> b) -> m a -> m b. This function allows you to convert any 
-- pure function into a function working on a monad (usually called lifting). As you already saw, this makes every Monad a Functor.
-- This goes on until you find a function with more than one parameter that you
-- want to be lifted. For that case, the Control.Monad module provides the
-- functions liftM2, liftM3 and so on which convert into monadic form functions
-- with two, three or more arguments.
--
-- However, it seems that the need of a family of functions, one per each
-- number of arguments, is not very coherent with the elegance that Haskell
-- code usually has. One would expect a solution that works for every number
-- of arguments.
--
-- This solutionj exists, and its called `ap`. This function has type Monad m
-- => m (a -> b) -> m a -> m b. This small change in signature allows you to
-- chain sevral of these functions together. For example,say you want to lift
-- the compare function. First, you wrap the entire function into a monad to
-- satisfy the type of the first argument. You do so via the following :
--
-- Prelude> :t return compare
-- return compare :: (Monad m, Ord a) => m (a -> a -> Ordering)
--
-- Next you use `ap` to feed the first argument. Then, you get back another
-- function that expects one parameter less. You can think of `ap` as a
-- replacement of ($) when using monads. So, assuming x has type m a , then you
-- get this:
--
-- return compare `ap` x :: (Monad m, Ord a) => m (a -> Ordering)
--
-- Finally, you can use `ap` to feed the last arugment and get the final
-- results.
--
-- As a rule of thumb, you can replace any call of the form liftMn f x1 x2 ...
-- xn by returning f `ap` x1 `ap` x2 `ap` ... `ap` xn. The ability to do so
-- will play an imporatnt role in another important Haskell type class,
-- Applicative.
--


