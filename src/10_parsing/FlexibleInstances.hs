{-# LANGUAGE FlexibleInstances #-}

instance Functor (Either Int) where
  fmap _ (Left n) = Left n
  fmap f (Right r) = Right (f r)

{-
  Examples include (Check the above defn to understand what's going on) : 

  fmap (== "cheeseburger") (Left 1 :: Either Int String)
  > Left 1
  fmap (== "cheeseburger") (Right "fries" :: Either Int String)
  > Right False
-}

