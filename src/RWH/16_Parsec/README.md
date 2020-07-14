Parsing a file, or data of various types is a common task for programmers. We
already learned about Haskell's support for regular expressions. Regular
expressions are nice for many tasks, but they rapidly become unwieldy, or
cannot be used at all, when dealing with a complex data format.

Parsec is a useful parser combinator library, with which we combine small
parsing functions to build more sophisticated parsers. Parsec provides some
simple parsing functions, as well as functions to tie them all together. It
should come as no surprise that this parser library for Haskell is built around
the notion of functions.

