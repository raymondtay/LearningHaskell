Haskell is a high-level language, a really high-level language. We can spend
our days programming entirely in abstractions, in monids, functors and
hylomorphisms, far removed from any specified hardware model of computation.
The language specification goes to great lengths to avoid prescribing any
particular evaluation model. These layers of abstraction let us treat HAskell
as a notation for computation itself, letting us concentrate on the essence of
the problem without getting bogged dfown in low-level implementation decisions.
We get to program in pure thought.

# Strict Data Types

Strict data types are another effective way to provide strictness information
to the compiler. By default, Haskell data types are lazy, but it is easy enough
to add strictness information to the fields of a data type that then propagate
through the program. We can declare a new strict pair type.

```haskell
data Pair a b = Pair !a !b
```

# Fusion

The final bottleneck in our program is the lazy list itself. While we can avoid
allocating it all at once, there is still memory traffic each time around the
loop, as we demand the next cons in the list, allocate it to the heap, operate
on it, and continue. The list type is also polymorphic, so the elemtns of the
list will be represented as heap-allocated `Double` values.

