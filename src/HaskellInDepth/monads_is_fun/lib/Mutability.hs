
module Mutability where

{-
  Imagine you need to count total number of files in the given directories including the files in their subdirectories.
  To accomplish that we will need to arrange a counter and increment it for every file met while traversing the directories
  hierarchy. Even though this is a rather imperative approach, we can still do it in Haskell by exploiting 
  some mutability to implement the counter.

  The functions provided by the IORef module is:
  - readIORef
  - writeIORef
  - modifyIORef
  and they are not atomic in the sense that their use in concurrent programs could lead to race conditions.
  The Data.IORef module provides atomic versions of them that are more suitable for concurrent setting.
  We will discuss this and other details in the chapter on concurrency later in this book.


-}

import Data.IORef
import Control.Monad

sumNumbers :: IO Int
sumNumbers = do
  s <- newIORef 0
  go s
  readIORef s
    where 
      go s = do
        putStr "Enter next integer number(empty line to finish): "
        n <- getLine
        when (not $ null n) $ do
          let num = read n
          modifyIORef' s (+ num)
          go s

