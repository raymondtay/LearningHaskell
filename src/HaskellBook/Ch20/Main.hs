{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

import Data.Text (Text)

class DoSomething a where
  something :: a -> IO ()

instance DoSomething String where
  something _ = putStrLn "String"

instance DoSomething Text where
  something _ = putStrLn "Text"

main :: IO ()
main = do
  something "hello"


