## Definitions

_Typeclass inheritance_ is when a typeclass has a superclass. 
This is a way of expressing that a typeclass requires _another_ 
typeclass to be available for a given type before you can write 
an instance.
`
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
`
Here the typeclass *Fractional* _inherits_ from _Num_. 
We could also say that _Num_ is a _superclass_ of *Fractional*. 
The long and short of it is that if you want to write an instance
of *Fractional* for some _a_, that type _a_, must already have an 
instance of _Num_ before you may do so.

