## Evaluation in Haskell

In Haskell, the subexpression 1 + 2 is not reduced to the value 3. Instead, we
create a "promise" that when the valu eof the expression `isOdd (1 + 2 )` is
needed, we will be able to compute it. The record that we use to track an
unevaluated expression is referred to as a thunk. This is all that happens: we
create a thunk and defer the actual evaluation untill it is really needed. If
the result of this expression is never subsequently used, we will not compute
its value at all.

Nonstrict evluation is often referred to as lazy evaluation.


Purity makes the job of understanding code easier. The behavior of a pure
function does not depend on the value of a global variable, or the contents of
a database or the state of a network connection. Pure code is inherently
modular: every function is self-contained and has a well-defined interface.


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

## About naming types and values

In Haskell, the names of types and values are independent of each other. We
use a type constructor (i.e. the type's name) only in a type declaration or a t
ype signature. WE use a value constructor only in actual code. Because these uses
are distinct, there is no ambiguity if we give a type cosntructor and value constructor
the same name. If we are writing a type signature, we must be referring to a type constructor.
If we are writing an expression, we must be using the value constructor.

## About pattern matching in Haskell

Let's use an example, which would be easier 
```
myNot True = False
myNot False = True
```
Haskell lets us define a function as a series of equations: these two clauses
are defining the behavior of the same function for different patterns of input.
On each line, the patterns are the items following the fucniton name, up untill the = sign.

When we apply myNot, the haskll runtime checks the value we supply aginast the value
constructor in the first pattern. This does not match, so it tries against the second pattern.
That match succeeds, so it uses the righthand side of that equation as the result of the funciton
application.

The order of matching is important as matching proceeds from top to bottom and stops
at the first success. Equations that are below a successful match have no effect.

```
mysum (x:xs) = x + mysum xs
mysum [] = 0
```

### Is List an acceptable list ?

In the book, the authors wrote 
<pre>
we can easily prove to ourselves that our `List a` type has the same shape 
as the List type [a]. To do this, we write a funciton that takes
any value of type [a] and produces a value of type List a:
```
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil
```

