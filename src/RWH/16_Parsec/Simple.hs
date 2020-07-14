-- {-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Data.List
import Control.Monad

class Foo a where
  foo :: a -> String

instance Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo

instance Foo Integer where
  foo = show

instance Foo Double where
  foo = show

instance Foo String where
  foo = (++) "☺"

instance Foo Char where
  foo c = [c,'_']

whatIsThis :: (Show a, Num a, Semigroup a) => Maybe a
whatIsThis = do
  a <- Just 4
  b <- Just 3
  return ((a+1) <> (b+1))


-- 
-- class Borked a where
--   bork :: a -> String
-- 
-- instance Borked Int where
--   bork = show
-- 
-- instance Borked (Int, Int) where
--   bork (a, b) = bork a ++ " <-> " ++ bork b
-- 
-- instance (Borked a, Borked b) => Borked (a, b) where
--   bork (a, b) = ">>" ++ bork a ++ " <-> " ++ bork b ++ "<<"
-- 
-- 
