type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x: xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), new1) = push 42 stack
    (v, new2) = pop new1
    in pop new2 
