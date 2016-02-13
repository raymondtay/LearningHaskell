{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

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

{-
instance TooMany Goats where
  tooMany (Goats n) = n > 42
-}
