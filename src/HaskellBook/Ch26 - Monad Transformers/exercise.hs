{-# LANGUAGE InstanceSigs #-}

module Chapter26 where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import Control.Monad.State
import System.IO

-- we only need to use return once because it's one big Monad
--
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int) 
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap 

-- A terminological point to keep in mind when reading about monad transformers
-- is that when Haskellers say "base monad" they usually mean what is
-- structurally outermost.
--
--
type MyType a = IO [Maybe a] -- base monad is IO


-- rDec is a function that should get its argument in the context of Reader and
-- return a value decremented by one. The book reveals a hint that Reader from
-- `transformers` is actually ReaderT of Identity and that `runReader` is a
-- convenience function throwing away the meaningless structure for you. Using
-- this piece of information and looking back at the codee i've done for
-- ReaderT then it becomes clear what i need to do.
--
-- Remember that Reader a a is actually r -> m a where m ∈ Functor,Applicative,
-- Monad. Knowing that the function to wrap the decremented value.
rDec :: Num a => Reader a a
rDec = ReaderT (\r -> pure (r -1))

-- *Chapter26 > runReader rDec 1
-- 0
-- *Chapter26 > runReader rDec 2
-- 1
-- *Chapter26 > fmap (runReader rDec ) [1..10]
-- [0,1,2,3,4,5,6,7,8,9]
--
--


-- rShow is show but in Reader; again, we need to know that ReaderT a Identity
-- String really means `Show a => (\a -> Identity String)`.
-- and `show :: Show a => a -> String`
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (\a -> Identity (show a)) -- non-point-free style

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT (Identity . show) -- point-free style (Identity. show) :: Show a => a -> Identity String

-- rPrintAndInc will first print the input with a greeting, then return the
-- input incremented by one.
--
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT (\r ->
  do
    putStr ("Hello there input: " ++ show r ++ "\n")
    return (r+1))

-- *Chapter26 Control.Monad.Identity> runReaderT rPrintAndInc 1
-- Hello there input: 1
-- 2
-- *Chapter26 Control.Monad.Identity> traverse (runReaderT rPrintAndInc ) [1..10]
-- Hello there input: 1
-- Hello there input: 2
-- Hello there input: 3
-- Hello there input: 4
-- Hello there input: 5
-- Hello there input: 6
-- Hello there input: 7
-- Hello there input: 8
-- Hello there input: 9
-- Hello there input: 10
-- [2,3,4,5,6,7,8,9,10,11]
-- *Chapter26 Control.Monad.Identity>


-- sPrintIncAccum first prints the input with a greeting, then puts the
-- incremented input as the new state, and returns the original input as a
-- String.
--
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT (\s ->
  do
    putStr ("Hello there input: " ++ show s ++ "\n")
    return (show s, s+1))

-- *Chapter26 Control.Monad.Identity Control.Monad.State> mapM (runStateT sPrintIncAccum ) [1..4]
-- Hello there input: 1
-- Hello there input: 2
-- Hello there input: 3
-- Hello there input: 4
-- [("1",2),("2",3),("3",4),("4",5)]
-- *Chapter26 Control.Monad.Identity Control.Monad.State>

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  guard $ isValid v
  return (Just v)


doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of 
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("cool, was very excite: " ++ e)



