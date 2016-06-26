# State

What if i need state ? In Haskell we have many means of representing
accessing, and modifying state. We can think of state as data that 
exists in addition to the inputs and outputs of our functions, data
that can potentially change after each function is evaluated.

In this chapter, i want to learn how to 

- Talk about state 
- Explore some ways of handling state in Haskell
- Learn how to use State and Monad instance 

# What is state ?

The State type in Haskell is a means of expressing "state" which may
change in the course of evaluating code without resort to mutation.
The monadic interface for State is, much as you have already seen, 
more of a convenience than a strict necessity for working with State.

Haskell allows me the option of capturing the idea and convenience of a value
which potentially changes with each computation without resorting to 
mutability. State is that "thing" that captures this idea and cleans up 
the bookkeeping required. If you need in-place mutation, then the `ST` type
is what you want, and i want to learn how to use it.


