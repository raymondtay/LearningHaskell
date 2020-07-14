
main = do
  contents <- getContents 
  print (sumFile contents) 
    where sumFile = sum . map read . words

{-

  [[Writing Lazy Functions]]
  
  An example of recurive functions but not tail-recursive:

  (++) :: [a] -> [a] -> [a]
  (x:xs) ++ ys = x : (xs ++ ys)
  []     ++ ys = ys

  In a strict langauge, if we evaluate "foo" ++ "bar", the entire list is constructued and 
  then returned. Non-strict evaluation defers much of the work untill it is needed.

  If we demand an element of the expression "foo" ++ "bar", the first pattern of the 
  function's definition matches, and we return the expression `x: (xs ++ ys)`. Because
  the `(:)` constructor is nonstrict, the evaluation of `xs ++ ys` can be deferred: 
  we generate more elements of the result at whatever rate they are demanded.

  When we generate more of the result, we'll no longer be using `x` so the garbage
  colelctor can reclaim it. Since we generate elements of the result on demand, and do 
  not hold onto parts that we are done with, the compiler can evaluate our code in 
  constant space.

-}
