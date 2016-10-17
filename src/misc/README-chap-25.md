# Composing Types

It's really about _Monad Transformers_ - the design philosophy, 
its principles and the practicalities of using them. For many 
programmers, monad transfomers are indistinguishable from _magic_,
so we want to approach them from both angles and demonstrate that they 
are both comprehensible via their types and quite practical in normal 
programming.

Functors and applicatives are both closed under composition: this
means that you can compose two functors (or two applicatives) And return 
another functor (or applicative). This is not true of Monads, however:
when you compose two monads, the result is not necessarily another Monad.

A Monad Transformer is a variant of an ordinary type that takes 
an additional type argument which is assumed to have a monad instance.
For example, _MaybeT_ is the transformer variant of the _Maybe_ type.
The transformer variant of a type gives us a _Monad_ instance that binds
over both bits of structure. This allows us to compose monads and combine
their effects. Gettig comfortable with monad transformers is important
to becoming proficient in Haskell, so we are going to take it pretty slowly
and go step by step. You won't necessarily want to start out early on defining
a bunch of transformer stacks yourself, but getting familiar is always advantageous.

The important thing is that Monad Transformers are never sum or product types;
they are always just a means of wrapping one extra layer of (monadic) structure 
around a type, so there is never a reason they couldn't be newtypes.

