module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
  | RegisteredUser Username AccountNumber

{-
 - Through the use of pattern matchnig, we were able to unpack the 
 - RegisteredUser value of the User type and vary behavior over the 
 - different constructors of types.
 -
 - This idea of unpacking and dispatching on data is important, so let
 - us examine another example.
 -}

printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username n) (AccountNumber x)) = 
  putStrLn $ n ++ " " ++ show x


