
module FamIOMonad
(doStrAction)
where

str2message :: String -> String
str2message input = "Data: " ++ input

str2action :: String -> IO ()
str2action = putStrLn . str2message

numbers :: [Int]
numbers = [1..10]

doStrAction :: IO ()
doStrAction = do
  str2action "Start of the program"
  mapM_ (str2action . show) numbers
  str2action "Done!"

