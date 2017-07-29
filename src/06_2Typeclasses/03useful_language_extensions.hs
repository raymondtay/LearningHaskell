{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Data.List

class Foo a where
  foo :: a -> String

instance Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo

instance Foo Char where 
  foo c = [c]
--
-- Need TypeSynonymInstances and FlexibleInstances and not like what the book
-- suggests which is 'TypeSynonymInstances and OverlappingInstances'
--
instance Foo String where
  foo = id

