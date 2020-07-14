
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Base (MyBase(..)) where

import System.IO (IOMode(..))

class Monad b => MyBase a b | b -> a where
  write :: a -> String -> b ()
  
  writeToConsole :: a -> String -> b ()
  writeToConsole a b = write a b >> write a "\n"

