-- the creation of this typeclass follows the assumption (false)
-- that the designers of Haskell forgot to include the definition for
-- defining equality.

-- Repeating the 'head' of the README file ...
-- Typeclasses define a set of functions that can have 
-- different implementations depending on the type of data they are given.
-- Note: You want to be aware that the names of the functions defined
--       to work on the typeclasses have global scope i.e. you cannot REPEAT names

-- For all types 'a',so long as 'a' is an instance of BasicEq
-- then 'isEqual' takes two parametrs of type a and returns a Bool.'
class BasicEq a where 
    isEqual :: a -> a -> Bool

-- the 'instance' definition demonstrates what it means to when applied 
-- to the data type which is 'Bool'
instance BasicEq Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False

class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isEqual2 x y = not (isNotEqual2 x y)
    isNotEqual2 :: a -> a -> Bool
    isNotEqual2 x y = not (isEqual2 x y)

-- type JSONError = String
class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right
