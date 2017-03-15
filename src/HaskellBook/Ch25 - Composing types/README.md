# Composing Types

Functors and applicatives are both closed under composition : this means that
you can compose two functors (or two applicatives) and return another Functor
(or applicative, as the case may be). This is not true of monads, however, when
you compose two monds, the result is not necessarily another monad.

However, there are many times in "real code" when composing monads is
desirable. Different monads allows us to work with different effects. Composing
monads allows you to build up computations with multiple effects. By stacking,
for exampl, a Maybe monad with an IO, you can be performing IO actions while
also building up computations that have a possibility of failure, handled by
the Maybe Monad.

A monad transformer is a variant of an ordnariy type that takes an
additional type argument which is assumed to have a monad instance. For
example, a MaybeT is the transformer variant of the Maybe type. The transformer
variant of a type gives us a Monad instance that binds over both bits of
structure. This allows us to compose monads and combine their effects. Getting
comfortable with monad transformers is important to becoming proficient in
Haskell, so we are going to take it pretty slowly and go step by step.

## Type contructors are functions

Type constructors can take other type constructors as arguments, too, just as
functions can take other functions as arguments. This is what allows us to
compose types.

