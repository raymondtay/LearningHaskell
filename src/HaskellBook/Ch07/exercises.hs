module Chapter07 where

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

-- As you compose more functions, you can see that nesting all the parentheses
-- would become tiresome. This operator allows us to do away with that. It also
-- allows us to write in an even more terse style known as "pointfree".
--
