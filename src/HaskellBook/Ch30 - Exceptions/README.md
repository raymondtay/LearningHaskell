# Exceptions

Raising exceptional conditions via such datatypes is not always ideal, however.
In some cases, exceptions can be faster by eliding repeated checks for an
adverse condition. Exceptions are not explicitly part of the itnerfaces you are
using, and that has immediat econsequences when trying to reason about the ways
in which your program could fail. 

Exception handling is a way to dealing with errors and giving the program some
alternate means of execution or termination should one arise.

The difference between `throw` and `throwIO` can be seen in the type:
```haskell
throwIO :: Exception e => e -> IO a
```

Partiality in the form of throwing an exception can be thought of as an effect.
The conventional way to throw an exception is to use `throwIO`, which has `IO`
in its result. This is the same thing as `throw`, but `throwIO` embeds the
exception in `IO`. You always handle exceptions in `IO`. Handling exceptions
must be done in `IO` even if they were thrown without an `IO` type. You almost
never want `throw` as it throws exceptions, without any warning in the type,
even in `IO`.

# Error handling with bottoms

One thing to watch out for is situations where you catch an exception for a
value that might be bottom. Due to non-strictness, the bottom  could have been
forced before or after your exception handler, so you might be surprised if you
expected either:

* that your exception hadnler was meant to catch the bottom or
* that no bottoms would cause your program to fail after having caught, say, a
  `SomeException`.

The proper coping mechanism for this is a glass of scotch and to realise the
following things:

* The exception handling mechanism is not for, nor should be used for, catching
  bottoms.
* Having caught an exception, even `SomeException`, without rethrowing an
  exception does not mean your program won't fail.


Example
```haskell
import Control.Exception

noWhammies :: IO (Either SomeException ())
noWhammies = try undefined

megaButtums :: IO (Either SomeException ())
megaButtums = try $ return undefined

```


