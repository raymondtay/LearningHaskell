
# Motivation : Boilerplate Avoidance

Monads provide a powerful way to build computations with effects. Each of the
standard monads is specialized to do exactly one thing. In real code, we often
need to be able to use several effects at once.

A monad transformer is similar to a regular monad, but it's not a standalone
entity. Instead, it modifies the behavior of an underlying monad. Most of the
moands in the `mtl` library have transformer equivalents. By conention, the
transformer version of a monad has the same name, with a `T` stuck on the end.
For example, the transformer equivalent of `State` is `StateT`; it adds a
mutable state to an underlying monad. The `WriterT` monad transformer makes it
possible to write data when stacked on top of another monad.

