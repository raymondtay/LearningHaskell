-- Usually, when we define or apply a function in haskell, we write the name of the 
-- function, followed by its arguments. This notation is referred to as prefix, because the 
-- name of the function comes before its arguments.
-- If a function or constructor takes two or more arguments, we have the option of using
-- it in infix form, where we place it between its first and second arguments. This allows us to 
-- use functions as infix operators. 
-- To define or apply a function or value constructor using infix notation, we enclose its 
-- name in backtick characters (Something known as backquotes). Here are some simple infix 
-- definitions of a function and a type

a `plus` b = a + b

data a `Pair` b = a `Pair` b deriving (Show)


