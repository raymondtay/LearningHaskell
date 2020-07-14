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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

import Data.List

class Borked a where
  bork :: a -> String
instance Borked Int where
  bork = show
instance Borked (Int,Int) where
  bork (a,b) = bork a ++ " , " ++ bork b
instance (Borked a, Borked b) => Borked (a,b) where
  bork (a, b) = ">> " ++ bork a ++ ", " ++ bork b ++ " <<"

class Foo a where
  foo :: a -> String 

instance Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo

instance Foo Char where
  foo c = [c]

instance Foo String where
  foo s = id s

{-

The following interpreter sessions illustrate the difference between newtype and data constructors
A type created with the "data" keyword has a bookkeeping cost at runtime, for example
in order to track which constructor created a value. A "newtype" value, otoh, can have
only one constructor and so does not need this overhead. This makes it more space and time 
efficient at runtime.

Because a newtype's constructor is used only at compile time and does not even
exist at runtime, pattern matching on undefined behaves differently for types defined
using "newtype" than for those that use "data".

"undefined" when evaluated at runtime results in crash! so its most appropriate
in using that in the following interpreter session

*Main> data DataInt = D Int
*Main> case D undefined of D _ -> 1
1
*Main> :t D undefined
D undefined :: DataInt
*Main> :t D
D :: Int -> DataInt
*Main> newtype NewTypeInt = N Int
*Main> case N undefined of N _ -> 1
1


We dont crash here because there's no constructor present at runtime, matching against
N _ is infact equivalent to matching the wildcard (_) and it succeeds everytime

*Main> case undefined of N _ -> 2
2
*Main> case undefined of D _ -> 2
*** Exception: Prelude.undefined
*Main>

-}

