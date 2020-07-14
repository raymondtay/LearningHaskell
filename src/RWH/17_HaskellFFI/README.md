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

### Concurrency and the Foreign Function Interface (FFI)

Haskell's FFI allows Haskell code to call, and be called by, foreign language
code (primarily C). Foreign languages also have their own threading models - In
C, there are POSIX and Win32 threads. When building your apps with FFI, make
sure the compiler option `-threaded` is _enabled_.

In Haskell's design, a foreign call may be executed in any OS thread, and
subsequent calls may even be executed in different OS threads. In most cases,
this isn't a problem, but sometimes it is; some foreign code must be called by
a particular OS thread. There are two situations where this happens:

- Libraries that allow only one OS thread to use their API. GUI libraries often
  fall into this category. Not only must the library be called by onlhy one OS
  thread, but it must often be one particular thread (e.g. the main thread).
  The Win32 GUI APIs are an example of this.

- APIs that use internal thread-local state. The best known example of this is
  OpenGL, which supports multi-threaded use but stores state between API calls
  in thread-local storage. Hence, subsequent calls must be made in the same OS
  thread; otherwise the latter call will see the wrong state.

To handle these requirements, Haskell has a concept of bound threads. A bound
thread is a Haskell thread/OS thread pair that guarantees that foreign calls
made by the Haskell thread always take place in the associated OS thread. A
bound thread is created by `forkOS`:

```haskell
  forkOS :: IO -> IO ThreadId
```

In a multithreaded call, it is entirely possible for `f` to be called by
multiple OS threads concurrently. The GHC runtime system supports this
(provided you use `-threaded`) with the following behavior: each call becomes a
new _bound thread_. That is, a new Haskell thread is created for each call, and
the Haskell thread is bound to the OS thread that made the call. Hence, any
further out-calls made by the Haskell thread will take place in the same OS
thread that made the original in-call. This turns out to be important for
dealing with GUI Callbacks. The gUI wants to run in the main OS thread only, so
when it makes a callback into Haskell, we need to ensure that GUI calls made by
the callback happen in the same OS thread that invoked the callback.


