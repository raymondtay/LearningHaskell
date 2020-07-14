{-# LANGUAGE ExistentialQuantification #-}

module UsingExceptions where

import Control.Exception

data MyE = ThisE | ThatE deriving (Show)

instance Exception MyE

-- Both versions are equivalent, thanks to haskell and the latter version in
-- `example2` is definitely more readable, arguably.
example1 = catch (throw ThisE) (\e -> putStrLn ("Caught: " ++ show (e :: MyE)))
example2 = throw ThisE `catch` \e -> putStrLn ("Caught: " ++ show (e :: MyE))

data BaseE = forall e . Exception e => BaseE e


