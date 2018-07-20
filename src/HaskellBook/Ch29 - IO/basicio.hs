
main = do
  putStrLn "Greetings! What is your name?"
  myname <- getLine
  putStrLn $ "Welcome to Haskell, " ++ show myname ++ "!" -- `show` is not necessary here


