# Metaprogramming

Template Haskell is a very powerful set of abstractions, some might say too
powerful. It effectively allows us to run arbitrary code at compile-time to
generate other Haskell code. You can do crazy things, like going off and
reading from the filesytem or doing network that informs how your code compiles
leading to non-deterministic builds.

## Quasiquotation 

Quasiequotation allows us to express "quoted" blocks of syntax that need not
necessarily be the syntax of the host language, but unlike just writing a giant
string it is instead parsed into some AST data type in the host language.
Notably values from the host languages can be injected into the custom language
via user-defined logic allowing information to flow between the two languages.

In practice, quasiquotation can be used to implement custom domain specific
languages or integrate with other general languages entirely via
code-generation.


