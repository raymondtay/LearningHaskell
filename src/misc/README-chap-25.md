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


The important thing is that Monad Transformers are never sum or product types;
they are always just a means of wrapping one extra layer of (monadic) structure 
around a type, so there is never a reason they couldn't be newtypes.












