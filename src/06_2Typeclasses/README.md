
The `newtype` keyword exists to give an existing type a new identity and it has
more restrictions on its uses than the `data` keyword. Specifically, a
`newtype` can have only one value constructor, which must have exactly one
field.

Brief recap of Haskell's three ways of introducing new names for types:

- The `data` keyword introduces a truly new algebraic data type.
- The `type` keyword gives us a synonym to use for an existing type. We can use
  the type and its synonym interchangeably.
- The `newtype` keyword gives an existing type a distinct identity. The
  original type and the new type are not interchangeable.

