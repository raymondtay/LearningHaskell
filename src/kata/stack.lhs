
Stack is a last-in-first-object data structure. 

> data Stack a = Stack [a] deriving Show

The pop function basically allows you to pop off the first
element of a stack

> pop :: (Num a) => Stack a -> (a, Stack a)
> pop (Stack [])     = (0, Stack [])
> pop (Stack (x:xs)) = (x, Stack xs)

The push function allows you to push a element of the same type
as the stack

> push :: (Num a) => a -> Stack a -> Stack a
> push e (Stack es) = Stack (e:es)

> peek :: (Num a) => Stack a -> (a, Stack a)
> peek (Stack []) = (0, Stack [])
> peek (Stack (x:xs)) = (x, Stack (x:xs))

