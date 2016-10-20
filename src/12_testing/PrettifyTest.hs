{-
  QuickCheck encourages an approach to testing where the developer specifies invariants 
  that should hold for any data we can throw at the code. To test the pretty printing
  library, then, we'll need a source of input data. To do this, we take advantage of the small
  combinator suite for building random data that QuickCheck provides via the 'Arbitrary' class.
  The class prvoides a function, 'arbitrary', to generate data of each type.
-}
data Doc = Empty 
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)


