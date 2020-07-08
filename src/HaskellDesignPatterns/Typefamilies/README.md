---
Type-level Programming
---

Type families bring functions to the type-level. Polymorphic kinds bring
polymorphism to the kind-level. Type promotion bring datatypes and type-safety
to the kind-level.

Two major problems of the Haskell kind system are solved by these extensions:

* The kind system is too __restrictive__ (because it lacks polymorphism)
  * **Solution:** Provide polymorphism on the kind level (**PolyKinds**)
* The kind system is too permissive (kinds are too vague)
  * **Solution:** Promote data types to kinds to simulate a type-system on the
    kind-level (**DataKinds**)

__Kind signatures__ signify the arity of parameterization of a type, that is,
the number and position of type parameters in a type. However, arity says
nothing about type and the Haskell kind system is __untyped__.

