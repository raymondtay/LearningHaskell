
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

import Control.Monad
import Control.Monad.ST
import Data.Foldable          (traverse_)
import Data.IORef             (newIORef, modifyIORef', readIORef)
import System.Environment     (getArgs)
import System.Directory.Extra (listContents, doesDirectoryExist)
import Control.Monad.Extra (whenM, ifM, zipWithM)
import Data.STRef

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
-- `ifM` evaluates a predicate and if True, evaluates the LHS otherwise 
-- goes for the RHS ; you can see this from its type signature =>
-- ifM : Monad m => m Bool -> m a -> m a -> m a
--
fileCount :: FilePath -> IO Int
fileCount fp = do
  cnt <- newIORef 0
  whenM (doesDirectoryExist fp) $ go cnt fp
  readIORef cnt
    where
      go cnt fp = listContents fp >>= traverse_ (processEntry cnt)
      processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)
      inc cnt = modifyIORef' cnt (+ 1)


-- Data.STRef refers to mutable references in the ST monad (Control.Monad.ST
-- monad that is)
-- A value of type STRef s a is a mutable variable in state thread s,
-- containing a value of type a.

helloWorld :: ST s String
helloWorld = do
  ref <- newSTRef "hello" -- build a new STRef in the current state thread
  x   <- readSTRef ref
  writeSTRef ref (x ++ "world")
  readSTRef ref

helloWorld_ :: ST s String
helloWorld_ = do
  ref <- newSTRef ""
  modifySTRef ref (const "world")
  modifySTRef ref (++ "!")
  modifySTRef ref ("Hello, " ++)
  readSTRef ref

countZeroesST :: [Int] -> Int
countZeroesST xs = runST $ do
  c <- newSTRef 0
  traverse_ (\x -> when (x == 0) $ inc c) xs
  readSTRef c
    where inc c = modifySTRef' c (+1)


comp1 :: ST s (STRef s Int)
comp1 = do
  newSTRef 0
comp2 :: STRef s Int -> ST s Int
comp2 ref = do
  readSTRef ref

-- the type inference algorithm will infer the same type 's' for them
-- and if we try somehow to run them independently, but use the reference
-- returned from comp1 in comp2; now the inferred types s and s1 are considered
-- by the type checker to be different and hence the compilation failed. This
-- is one example of the type checker controlling the logic behind our code by
-- preventing parts of different ST computations to mixed in one.
-- result = runST (comp2 (runST comp1)) -- NOK
result' = runST (comp1 >>= comp2)

