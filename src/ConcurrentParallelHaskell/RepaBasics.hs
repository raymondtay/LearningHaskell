

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

{--
  Advice for writing fast code:
  1. Repa does not support nested parallelism. This means that you cannot map a parallel worker function across an array
     and then call computeP to evaluate it, or pass a parallel worker to parallel reductions such as foldP. If you do then you 
     will get a run-time warning and the code will run very slowly.

  2. Arrays of type (Array D sh a) or (Array C sh a) are not real arrays. They are represented as functions that compute each
     element on demand. You need to use computeS, computeP, computeUnboxedP and so on to actually evaluate the elements.

  3. Add INLINE pragmas to all leaf-functions in your code, especially one that computes numeric results. Non-inlined lazy
     function calls can cost upwards of 50 cycles each, whiel each numeric operator only costs one (or less). Inlining 
     leaf functions also ensures they are specialized at the appropriate numeric types.

  4. Add bang patterns to all functions arguments, and all fields of your data types. In a high-performance Haskell program, the cost
     of lazy evaluation can easily dominate the runtime if not handled correctly. You don't want to rely on the strictness 
     analyser in numeric code because if it does not return a perfect result thne the performance of your program will be awful. This
     is less of a problem for general Haskell code, and in a different context relying on strictness analysis is fine.

  5. Scheduling an 8-thread parallel computation can take 50 microseconds on a Linux machine. You should switch to sequential evaluation
     of functions like computeS and foldS for small arrays in inner loops, and at the bottom of a divide-and-conquer algorithm.
     Consider using a compu9teP that evaluates an array defined using computeS or foldS for each element.

  6. Compile the modules that use Repa with the following flags: -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000
     -fllvm-optlo-03. You don't want the liberate-case transform because it tends to duplicate too much intermediate code, and is not needed if you
     use bang patterns as per point 4. The unfolding flags tell the inliner to not to fool around with heuristics, and just inline everything.
     If the binaries become too big then split the array part of your program into separate modules and only compile those with the unfolding flags.

  7. Repa writes to the GHC eventlog at the start and end of each parallel computation. Use threadscope to see what your program is doing.

  8. When you are sure your program works, switch to the unsafe versions of functions like traverse. These don't do bounds checks.


--}


