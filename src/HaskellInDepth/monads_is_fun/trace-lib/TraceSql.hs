{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module TraceSql where

import Data.Foldable
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data ErrorMsg = WrongFormat Int T.Text deriving Show

type SQL = T.Text

genInsert s1 s2 = "Insert into items values ('" <> s1 <> "', '" <> s2 <> "');\n"

processOneLine :: (Int, T.Text) -> Writer [ErrorMsg] SQL
processOneLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2 -- ViewPatterns ghc extension
processOneLine (i, s) = writer (T.empty, [WrongFormat i s])

genSQL :: T.Text -> Writer [ErrorMsg] SQL 
genSQL t = T.concat <$> traverse processOneLine (zip [1..] $ T.lines t)

testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL = do
  let (sql, errors) = runWriter (genSQL testData)
  TIO.putStrLn "SQL: "
  TIO.putStr sql
  TIO.putStrLn "Errors: "
  traverse_ print errors

type Counter = Sum Int
gcd_count :: Integer -> Integer -> Writer Counter Integer
gcd_count a 0 = tell (Sum 1) >> pure a
gcd_count a b = tell (Sum 1) >> gcd_count b (a `mod` b)

type CountingSteps = [(Integer, Integer)]
gcd_count' :: Integer -> Integer -> Writer CountingSteps Integer
gcd_count' a 0 = tell [(a, 0)] >> pure a
gcd_count' a b = tell [(a, b)] >> gcd_count' b (a `mod` b)

gcd_count'' = (mapWriter (Sum . length <$>) . ) . gcd_count'


