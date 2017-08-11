There are a few terms of art around Monads that you may not be familiar with.
These aren't formal, but they're commonly used, so its helpful to know about
them:
- Monadic simply means "pertaining to Monads". A monadic type is an isntance of
  the Monad typeclass; a monadic value has a monadic type. 

- When we say that a type is a monad; this is really a shorthand way of saying
  that it's an instance of the Monad typeclass. Being an instance of Monad
  gives us the necessary monadic triple of type constructor, injection
  funciton, and chaining function.

- In the same way, a reference to "the Foo monad" implies that we are talking
  about the type named Foo and that it's an instance of Monad.

- An action is another name for a monadic value. This use of the word probably
  originated with the introduction of monads for I/O, where a monadic value
  such as print "foo" can have an observable side effect. A function with a
  monadic return type might also be inferred to as an action, though this is a
  little less common.


The `Monad` typeclass does not provide any means for values to escape their
monadic shackles. We can inject a value into a monad using `return`. We can
extract a value from a monad using `(>>=)` but the function on the right, which
can see an unwrapped value, has to wrap its own result back up agai.

Most monads have one or more `runLogger`-like functions. The notable exception
is of course `IO`, which we usually escape from simply by exiting a program.

A monad execution function runs the code inside the monad and unwraps its
result. Such functions are usually the only means provided for a value to
escape from its monadic wrapper. The author of a monad thus has complete
control over how whatever happens inside the monad gets out.


