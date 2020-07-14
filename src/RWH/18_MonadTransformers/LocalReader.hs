{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader
import Data.Functor.Identity

myName :: String -> ReaderT String Identity String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second") -- use 'local' to modify the result "locally".
  c <- myName "Third"
  return (a, b, c)

{-
  Here's how it would look like when i ran it:

  *Main Control.Monad Control.Monad.Trans> runReader localExample "Ray"
  ("First, I am Ray","Second, I am Raydy","Third, I am Ray")
  *Main Control.Monad Control.Monad.Trans>

-}

