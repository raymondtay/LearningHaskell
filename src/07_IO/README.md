## About I/O in Haskell

### What is an I/O Action?

+ Have the type 'IO t'
+ Are first-class values in Haskell and git seamlessly with Haskell's type system
+ Produce an effect when performed, but not when evaluated. That is, they
  produce an effect only when called by something else in an I/O context.
+ Any expression may produce an action as its value, but the action will not 
  perform I/O until it is executed inside another I/O action (or it is 'main')
+ Performing (Executing) an action of type 'IO t' may perform I/O and
  will ultimately deliver a result of type 't'

### Why Purity Matters?

Haskell draws a clear distinction between pure code and I/O actions. In languages such as C or Java, 
there is no such thing as a function that is guaranteed by the compiler to always return the
same result for the same arguments or a function that is guaranteed to never have side effects.
The only way to know if a given function has side effects is to read its documentation and hope
its accurate.

Many bugs in programs are caused by unanticipated side effects. Still more are caused by misunderstanding
circumstances in which functions may return different results for the same input. As multithreading 
and other forms of parallelism grow increasingly common, it becomes more difficult to manage 
global side effects.

Haskell's method of isolating side effects into I/O actions provides a clear boundary.
You can always know which parts of the system may alter state and which won't. You can always 
be sure that the pure parts of your program aren't having unanticipated results. This helps you
to think about the program. It also helps the compiler to think about it. Recent versions of ghc, 
for instance can provide a level of automatic parallelism for the pure parts of your code - something
of a holy grail for computing.

### About hGetContents

One novel way to approach I/O is with hGetContents function. hGetContents has
the type Handle -> IO String. The String it returns represents all of the data in the file
gtiven by the Handle.

In a strictly evaluated language, using such a funciton is often a bad idea. It may be 
find to read the entire contents of a 2 KB file but if you try to read the netire contents
of a 500GB file, you are likely to crash due to lack of RAM to store all that data. In 
these langauges, you would traditionally use mechanisms such as loops to process the file's
entire data.

But hGetContents is differnt. The String it returns is evaluated lazily. At the moment
you call hGetContents, nothing is actually read. Data is only read from the HJandle as the elements
of the list are processed. As elements of the String are no longer used, Haskell's garbage collector
automatically frees that memory. all of this happens completely transparently to you
And sicne you have what looks like (And really is) a pure string, you can pass it to pure Non-IO code.


