{- 
  Normally, we cannot write an instance of a typeclass for a specialized version of a
  polymorphic type. The [Char] type is the polymorphic type [a] specialized to the type
  Char. We are thus prohibited from declaring [Char] to be an instance of a typeclass.
  This is highly inconvenient, since strings are ubiquitous in read code.
  The TypeSynonymInstances language extension removes this restriction permitting us
  to write such instances.

  GHC supports another useful language extension. OverlappingInstances, which
  addresses the problem we saw with overlapping instances. When there are multiple overlapping
  instances to choose from, this extension causes the compiler to pick the most specific one.
-}

{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import Data.List

class Foo a where
  foo :: a -> String 

instance Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo

instance Foo Char where
  foo c = [c]

