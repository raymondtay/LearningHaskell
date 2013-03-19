-- This function demonstrates a basic definition of a function
-- and in our case we want to mimick the behavior of 'drop'
-- You'll notice error checking for for its dependent arguments 
-- are done and using of a in-built function 'tail'

myDrop :: (Num a, Ord a) => a -> [a1] -> [a1]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

