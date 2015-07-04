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


  Two basic rules about functors and they are:
  + functors must preserve identity. That is, applying `fmap id` to a value 
    should give us back an identical value.
  + functors must be composable. That is, composing two uses of
    fmap should give the same result as one fmap with the same functions composed

-}

