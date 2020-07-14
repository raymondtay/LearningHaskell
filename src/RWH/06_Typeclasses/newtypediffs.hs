
-- newtype has more restrictions on its use than the data keyword

data TwoFields = TwoFields Int Int

newtype Okay = ExactlyOne Int

newtype Param a b = Param (Either a b)

newtype Record = Record {
    getInt :: Int
}

-- NOK: too little or no fields
-- newtype TwoFew = TooFew

-- NOK: more than one field
-- newtype TooManyFields = Fields Int Int

-- NOK: newtype allows exactly one constructor
-- newtype TooManyCtors = Bad Int | Worse Int 
