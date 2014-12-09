
name2reply :: String -> String
name2reply name = 
    "pleased to meet you, " ++ name ++ ".\n" ++
    "your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

main :: IO ()
main = do
    putStrLn "Greetings once again.What is your name?"
    input <- getLine
    let nname = name2reply input
    putStrLn nname

-- to run program, "$> runghc ./io_from_pureio.hs"

