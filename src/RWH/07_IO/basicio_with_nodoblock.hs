main = 
    putStrLn "Greetings! Your name please?" >> 
    getLine >>= (\name -> putStrLn $ "Welcome to Haskell, " ++ name ++ "!")

