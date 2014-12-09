
-- classic example.
-- to run this on the commandline prompt: "runghc ./basic_io.hs"
--
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn $ "Welcome to Haskell, " ++ name ++ "!"

-- Actions can be created, assigned and passed anywhere. However, they 
-- may only be performed and executed from within another I/O action.

