{-# LANGUAGE GADTs #-}
import Control.Applicative (liftA2)
import Control.Monad       (liftM)
import Data.Either
import Numeric

-- data Expr = I Int | Add Expr Expr | Mul Expr Expr
-- eval :: Expr -> Int
-- eval (I n) = n
-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Mul e1 e2) = eval e1 * eval e2

-- data Expr = I Int | B Bool | Add Expr Expr | Mul Expr Expr | Eq Expr Expr

-- This version is not quite right either...
-- eval :: Expr -> Either Int Bool
-- eval (I n) = Left n
-- eval (B n) = Right n
-- eval (Add e1 e2) = undefined -- what does it mean for bool + bool ? or int + bool or bool + int ? clearly, it does not make sense and it does not compute ... so we have to capture that as well
-- eval (Mul e1 e2) = undefined -- same reasoning here
-- eval (Eq e1 e2) = undefined

-- The following is how it might work, possibly, according to my
-- interpretation.
-- eval :: Expr -> Maybe (Either Bool Int)
-- eval (I n) = Just (Right n)
-- eval (B n) = Just (Left n)
-- eval (Add (I e1) (I e2)) = Just . Right $ (e1 + e2)
-- eval (Mul (I e1) (I e2)) = Just . Right $ (e1 * e2)
-- eval (Add (B e1) (B e2)) = Just . Left  $ (e1 && e2) -- i choose to interpret "add" == "&&"
-- eval (Mul (B e1) (B e2)) = Just . Left  $ (e1 || e2) -- i choose to interpret "or" == "||"
-- eval (Add e1 e2) = (<*>) (fmap (liftA2 (+)) (eval e1)) (eval e2)
-- eval (Mul e1 e2) = (<*>) (fmap (liftA2 (*)) (eval e1)) (eval e2)
-- eval (Eq e1 e2)  = let a = fmap (fromLeft False) (eval e1)
--                        b = fmap (fromLeft False) (eval e2)
--                     in liftM Left ((<*>) (fmap (==) a) b)

-- main :: IO ()
-- main = do
--   putStrLn . show $ eval (Add (I 32) (I 10))
--   putStrLn . show $ eval (Add (Add (I 32) (I 10)) (I 33))
--   putStrLn . show $ eval (Eq (Add (B False) (B True)) (B False))
--   putStrLn . show $ eval (Eq (Mul (B False) (B True)) (B False))
--   putStrLn . show $ eval (Eq (Add (I 32) (I 10)) (I 42))
--   putStrLn . show $ eval (Eq (Add (I 33) (I 10)) (I 42)) -- this is doomed! examples of "it compiles but does not give the correct result"

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Eq a => Expr a -> Expr a -> Expr Bool

eval :: Expr a -> a
eval (I n) = n
eval (B n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

data Bit = O | Z deriving Show

serialise :: Expr a -> [Bit]
serialise (I x) = foldl (\acc -> \e -> if e == '1' then O:acc else Z:acc) [] $ showIntAtBase 2 ("01" !!) x ""
serialise (B True) = [O]
serialise (B False) = [Z]
serialise (Add e1 e2) = serialise $ I ((eval e1) + (eval e2))
serialise (Mul e1 e2) = serialise $ I ((eval e1) * (eval e2))

main :: IO ()
main = do
  putStrLn . show $ eval (Add (I 32) (I 10))
  putStrLn . show $ eval (Add (Add (I 32) (I 10)) (I 33))
-- putStrLn . show $ eval (Eq (Add (B False) (B True)) (B False)) -- no longer allowed
-- putStrLn . show $ eval (Eq (Mul (B False) (B True)) (B False)) -- no longer allowed
  putStrLn . show $ eval (Eq (Add (I 32) (I 10)) (I 42))
  putStrLn . show $ eval (Eq (Add (I 33) (I 10)) (I 42))
  putStrLn . show $ serialise (Add (Add (I 33) (I 10)) (I 42))
  putStrLn . show $ serialise (Mul (Add (I 33) (I 10)) (I 42))

