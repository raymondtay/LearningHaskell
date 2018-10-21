import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe
import Control.DeepSeq (force)
import Control.Parallel.Strategies

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      (as, bs) = splitAt (length puzzles `div` 2) puzzles
      solutions = runEval $ do
        as' <- rpar (force (map solve as))
        bs' <- rpar (force (map solve bs))
        rseq as'
        rseq bs'
        return (as' ++ bs')

  print (length (filter isJust solutions))
