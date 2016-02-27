module Hello where

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hello '" ++ name ++ "' from Haskell")


