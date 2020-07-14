## about functional programming in haskell

Below is an example defining and usage of a function declared in a let-block with the where-clause

```
*Main> let square2 xs = map sq xs where sq x = x * x in square2 [1..2]
[1,4]
it :: (Num b, Enum b) => [b]
(0.01 secs, 3095848 bytes)
```
### Why use Folds, Maps and Filters?

Quoting the book:
<pre>
The advantage her elies in the fact that folds are extremely common 
in Haskell and they have reguylar and predictable behavior.
This means that a reader with a little experience will have an easier time
understanding a use of a fold than code that uses explicit recursion. A fold
isn't going to produce any surprises, but the behavior of a function that recurses
explicityly isn't immediately obvious. Explicit recursion requires us to read closely to understand
exactly what's going on.
</pre>

Here's an example on how to reduce the sum 
```
foldl (+) 0 [1..100]
foldr (+) 0 [1..100]
```
and that's conducting the reduction from left-right and
the second expression conducts the same thing from right-left.
From experience, it definitely helps when you can visualize what 
is exactly going on in the system.

following our previously expressed desire to visualize the entire process
let's start it by folding-left
```
foldl (+) 0 [1..5]
=> foldl (+) (0 + 1) [2..5]
=> foldl (+) ((0 + 1) + 2) [3..5]
=> foldl (+) (((0 + 1) + 2) + 3) [3..5]
=> foldl (+) ((((0 + 1) + 2) + 3) + 4) [4..5]
=> foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) []
```
and similarly,we approach the visualization of the folding process
from the right
```
foldr (+) 0 [1..5]
=> 1 + foldr (+) 0 [2..5]
=> 1 + (2 + foldr (+) 0 [3..5])
=> 1 + (2 + (3 + (foldr (+) 0 [4..5])))
=> 1 + (2 + (3 + (4 + (foldr (+) 0 [5..5]))))
=> 1 + (2 + (3 + (4 + (5 + foldr (+) 0 []))))
```
As in the book, the class of functions that we can express using foldr is called primitive recursive.

### About Sections in Haskell 

Haskell provides a handy notational shortcut to let us write a partially applied
function to infix style. If we enclose an operator in parentheses, we can supply 
its left or right argument inside the parentheses to get a partially applied function.
This is called a Section.

### Developing Haskell Code without Going Nuts

Early on, we we come to grips with Haskell development, we have so many new
unfamiliar concepts to keep track of at one time that it can be a challenge to 
write code that compiles at all. 

One useful technique for quickly developing the skeleton of a program is to write
placeholder, or stub, versions of types and functions.
