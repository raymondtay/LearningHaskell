
The type of `putStrLn` looks like a function. It takes a parameter of type
`String` and returns value of type `IO ()`. Just what is an `IO ()` though?

Anything that is type `IO something` is an I/O `action`. You can store it and
nothing will happen. I could say something like `let xxx = putStrLn "hello
world"` and nothing happens till you evaluate it like this: 
```
> let xxx = putStrLn "hello world"
> do xxx
"hello world"
```

# What is an I/O action?

Actions:

- Have the type `IO t` where t ∈ "something we dont know yet"
- Are first-class values in Haskell and fit seamlessly with Haskell's type
  system.
- Produce an effect when performed, but not when evaluated. That is, they
  produce an effect only when called by something else in an I/O context.
- Any expression may produce an action as its value, but the action will not
  perform I/O untill it is executed inside another I/O action (or it is main).
- Performing (executing) an action of type `IO t` may perform I/O and will
  ultimately deliver a result of type `t`.

## Is Haskell really imperative ??
These `do` blocks may look a lot like an imperative language. After all, you
are giving commands to run in sequence most of the time. 

But Haskell remains a lazy language at its core. While it is sometimes
necessary to sequence actions for I/O, this is done using tools that are part
of the Haskell ecosystem already. Haskell achieves a nice separation of I/O
from the rest of the langauge through the IO monad as well.
