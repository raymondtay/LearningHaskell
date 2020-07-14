{-
  QuickCheck encourages an approach to testing where the developer specifies invariants 
  that should hold for any data we can throw at the code. To test the pretty printing
  library, then, we'll need a source of input data. To do this, we take advantage of the small
  combinator suite for building random data that QuickCheck provides via the 'Arbitrary' class.
  The class prvoides a function, 'arbitrary', to generate data of each type.
-}
import Test.QuickCheck.All

data Ternary = Yes
  | No | Unknown deriving (Eq, Show)

instance Arbitrary Ternary where 
  arbitrary = elements [Yes, No, Unknown]

instance Arbitrary Ternary where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    return $ case n of
        0 -> Yes
        1 -> No
        _ -> Unknown


