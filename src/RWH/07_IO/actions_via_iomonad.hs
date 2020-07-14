{-
	Why a mapM when we already have map? Because map is a pure function that
	returns a list. It doesn't and cannot actually execute actions directly. mapM is
	a utility that lives in the IO monad and thus can actually execute the actions.
	Going back to main, mapM_ applies (str2action . show) to every element in numbers. 
	show converts each number to a String and str2action convers each string to an action. 
	mapM_ combines these individual actions into one big aciton that prints out lines.
    
    Technically speaking, mapM combines a bunch of separate I/O actions 
    into one big action. The separate actions are executed when the big action is.
-}
str2message :: String -> String
str2message input = "Data : " ++ input

str2action :: String -> IO ()
str2action = putStrLn . str2message

numbers :: [Int]
numbers = [1..10]

main = do 
    str2action "Start of the program:"
    mapM_ (str2action . show) numbers 
    str2action "Done!"

