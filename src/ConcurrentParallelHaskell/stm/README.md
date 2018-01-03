
STM provides a subtrate for concurrent programming that offers far richer
composition than has been available to date, and that it can be implemented in
a practical language.

We have used Haskell as a particularly suitable laboratory, but an obvious
question is this: to what extent can our results be carried back into the
mainstream world of imperative programming? We believe that the idea of using
constructs like `retry` and `orElse` can indeed be applied to other languages.
For instance, in C# one could indicate `retry` by raising a specific kind of
exception and then express `orElse` as a particular kind of exception handler.


