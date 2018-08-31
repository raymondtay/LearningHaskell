# Exceptions

Raising exceptional conditions via such datatypes is not always ideal, however.
In some cases, exceptions can be faster by eliding repeated checks for an
adverse condition. Exceptions are not explicitly part of the itnerfaces you are
using, and that has immediat econsequences when trying to reason about the ways
in which your program could fail. 

Exception handling is a way to dealing with errors and giving the program some
alternate means of execution or termination should one arise.

