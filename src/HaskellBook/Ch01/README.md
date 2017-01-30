# What is functional programming ?

Functional programming is a computer programming paradigm that relies on functions modeleled on mathematical functions.
The essence of functional programming is that programs are a combination of `expressions`.
Expressions include concrete values, variables and also functions.

## Beta reduction

When we apply a function to an argument, we substitute the input expression for all instances of bound variables
within the body of the abstraction. You also eliminate the head of the abstraction, since its only purpose was to bind a variable. This process is called `beta reduction`.

Beta reduction is this process of applying a lambda term to an argumne,t replacing the bound variables with the value of the argument and eliminating the head.

## Free variables

The purpose of the head of the function is to tell us which 
variables to replace when we apply our function, that is , to 
bind the variables. A bound variable must have the same value throughout the expression. But sometimes the body expressions has variables that are not named in the head . We call those variables `free variables`. 
```scala

λx.xy // x is the `bound` variable, y is the free variable.

```

Haskell is a `typed` lambda calculus with a lot of surface level decoration sprinkled on top, to make it easier for humans to write, but the semantics of the core language are the same as the lambda calculus.

## Notes

- When you write code in a source file, the order is unimporant but when writing code directly into the REPL then order makes a lot of difference.


