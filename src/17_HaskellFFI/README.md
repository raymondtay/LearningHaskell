# Background

Programming languages do not exist in isolation. They inhabit an ecosystem of
tools and libraries, built over over decades, and often written in a range of
programming languages. 

The Haskell Foreign Function Interface (FFI) is a means by which Haskell code
can use, and be used by, code written in other languages. In this chapter, we
will look at how the FFI works and how to produce a Haskell binding to a C
library.

The challenge: take PCRE, the standard Perl-compatible regular expression
library and make it usable from HAskell in an efficient and functional way.
Throughout, we will seek to abstract out manual effort required by the C
implementation, delegating that work to HAskell to make the interface more
robust yield a clean, high-level binding. We assume only basic familiarity with
regular expressions.

