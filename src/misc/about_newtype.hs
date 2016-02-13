{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module AboutNewType where

{-
 - Intermission exercises from Chapter 11
 - of the book Haskell Programming 
 -}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany pair = fst pair > 42

instance TooMany (Int, Int) where
  tooMany pair = (+) (fst pair) (snd pair) > 42

-- ignore the first element of the pair
instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany pair = tooMany (snd pair)

newtype Goats = Goats Int deriving (Show, TooMany)

{- we can skip the following expression once we have GeneralizedNewtypeDeriving
instance TooMany Goats where
  tooMany (Goats n) = n > 42
-}


data XProduct a b = XProduct a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

type Product' a = XProduct a Int

newtype YProduct = Product' Int 

