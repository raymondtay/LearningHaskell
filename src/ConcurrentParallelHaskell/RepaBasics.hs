

import Data.Array.Repa as Repa

create_array :: Array U DIM2 Int
create_array = fromListUnboxed (Z:.3:.5) [1..15] :: Array U DIM2 Int

get :: Int -> Int -> Array U DIM2 Int -> Int
get r c arr = arr ! (Z:.r:.c)

mapArr f arr   = Repa.map f arr
yonedaDemo f g = Repa.map f . Repa.map g
mymap f a      = Repa.fromFunction (extent a) (\ix -> f (a ! ix))

main = do
  let arr = create_array
  putStrLn $ "Referencing (2,1) from repa of 15 elements of shape " Prelude.++ show (extent arr) Prelude.++ " i => " Prelude.++ show (get 2 1 arr)
  let arr2 = computeS . mapArr (+1) $ arr :: Array U DIM2 Int
  let arr3 = computeS ( yonedaDemo (+1) (^3) arr ):: Array U DIM2 Int
  let arr4 = computeS ( mymap ((+1).(^3)) arr ):: Array U DIM2 Int
  putStrLn $ "Elements of array after adding 1 to each element => " Prelude.++ show arr2
  putStrLn $ "Elements of array after adding 1 followed by applying a power function to each element => " Prelude.++ show arr3
  putStrLn $ "Elements of array after adding 1 followed by applying a power function to each element => " Prelude.++ show arr3
  putStrLn $ "Elements of array after adding 1 followed by applying a power function to each element => " Prelude.++ show arr4

