import Data.Char(toUpper)

isGreen :: IO Bool
isGreen = 
    do 
        putStrLn "Is green your fav color?"
        answer <- getLine
        return ((toUpper . head $ answer ) == 'Y')

-- We have a pure computation that returns a bool. That computaiton is passed to 
-- return, which puts it into the IO monad. Since it is the last value in the do block
-- it becomes the return value of isGreen, but this is not because we used the return
-- function.

