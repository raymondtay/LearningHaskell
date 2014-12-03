
## About Parameteric Polymorphism

When a function has type variables in its signature, indicating that some of its arguments
can be of any type, we call the function polymorphic.

As an example, take this function `f` which takes in a list of a and returns a list of a
```
f :: [a] -> [a] 
```
from the book ... 

the type variable, a, defines a placeholder if you will for the type parameter
And if a type contains type parameters, we say that it is a parameterized type or a polymorphic
type. If a function or value's type contains type parameters, we call it polymorphic.

When we see a parmameterized type, we have already noted that the code
doesn't care what the actual type is. Howver, we can make a stronger statement:
it has no way to find out what the real type,or to manipulat avalue of that type. It cannot
create a value; netiehr can it inspect one. All it can do is treat it as afully abstract black box.

Parameteric polymorphism is the most visible kind of polymorphism that Haskell 
supports. Haskell's parameteric polymorphism directly influenced the desing
of the generic facilities of the Java and C# lagnauges. A parameterized type
in Haskell is similar to a type variable in Java generics. C++ templaets 
also bear a resemblance to parameteric polymorphism

## Why the fuss over purity?

Haskell's insistence on purity has profound and valuable consequences. Because the result of
applying a pure funciton can only depend on its arguments, we 
can often get a good hint of what a pure function does by simply reading 
its name and understanding its type signature. 

Purity makes the ob of understanding code easier. The behavior of a puer function
does not depend on the value of a global variable, or the contents of a database
or the state of a network connection. Pure code is inherently modular: 
every funciton is self-contained and has a well defined structure.

