
module GreetIfCool where

greetIfcool :: String -> IO ()
greetIfcool coolness =
  if cool coolness
    then putStrLn "Eyyyy, what's up? "
  else
    putStrLn "phssshsh"
  where cool v = v == "yes"

