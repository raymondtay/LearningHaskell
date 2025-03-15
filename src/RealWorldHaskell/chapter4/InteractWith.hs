module InteractWith where

import System.Environment (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

runInteractWith :: IO ()
runInteractWith = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        -- replace "id" with the name of our function below
        myFunction = id

main :: IO ()
main = do
  runInteractWith

