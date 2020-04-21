
import Control.Monad
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

-- load this program into ghci
-- *Main> main
-- [[2013,501,2558],[2013,1004,2558]]
--
main :: IO ()
main = do
  let graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]
  putStrLn . show $ pathsWriter graph1 2013 2558
