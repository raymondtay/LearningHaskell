
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

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
-- cmonad computation should be similar to seimilar to execStateT (execState
-- factorial x 1 )
