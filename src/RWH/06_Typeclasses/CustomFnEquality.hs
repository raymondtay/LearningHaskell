
data MyType = MyType (Int -> Bool) -- the data and value constructors are the same name

instance Show MyType where -- the syntax is 'instance <typeclass name> <data constructor name>' 
    show (MyType f) = "A function of Int -> Bool" -- the syntax needs to be related to the value constructor
