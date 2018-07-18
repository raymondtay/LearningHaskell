module WhatHappens where

import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero

{-
  When i ran the above program in ghci, here's what i saw 

  *WhatHappens> :main
  *** Exception: thread blocked indefinitely in an MVar operation
  *WhatHappens>

  Best explained in the book : The problem here is that the type `IO MVar a` of `newEmptyMVar` is a recipe
  for producing as many empty MVars as you need or want; it is not a reference to a single, shared MVar. In
  other words, the two references to myData here are not referring to the same MVar.

  Taking from an empty MVar blocks until something is put into the MVar
-}
