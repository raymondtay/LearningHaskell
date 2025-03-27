{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.List


class Foo a where
  foo :: a -> String

instance Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo

instance Foo Char where
  foo c = [c]

instance Foo String where
  foo = id


-- The purpose of newtype declaration is to rename an existing type, giving it
-- a distinct identity. If a newtype does not use automatic deriving to expose
-- the underlying type's implementation of a typeclass, we are free to either
-- write a new instance or leave the typeclass unimplemented.
data DataInt = D Int deriving (Eq, Ord, Show)
newtype NewtypeInt = N Int deriving (Eq, Ord, Show)

