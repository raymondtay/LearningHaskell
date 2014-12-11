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

You are not required to ever consume all the data from the input file when using 
hGetContents. Whenever the Haskell system determines that the entire string
hGetContents returned can be garbage collected, the file is closed for you automatically.
The same principle applies to data read from the file. Whenver a piece of data will never again
be needed, the haskell environment releases the memory it stored within. 

Strictly speaking, we dont really need to call hClose ... but its a good practice anyway.

When using hGetContents, it is important to remember that even though you 
may never agian explicitly reference Handle directly in the rest of the 
program, you must not close the Handle untill you have finished consuming 
its results via hGetContents. Doing so would cause you to 
miss on some or all of the file's data. Since Haskell is lazy, you 
generally can assume that you have consumed input only after you have
output the result of the computaitons involving the input.

### About Lazy Output

As before, nothing in Haskell is evaluated before its value is needed. Since functions
such as writeFile and putStr write out the entire String passed to them, that 
entire String must be evaluated. So you are guaranteed that the argument to putStr will be evaluated
in full.

But what does that mean for laziness of the input? In the earlier examples, will the
call to putStr or writeFile force the entire input String to be loaded into memory at once, just to be
written out?

The answer is no. putStr (and all similar output functions) write out data as it
becomes available. They also have no need for keeping around data already written, so
as long as nothing else in the program needs it, the memory can be freed immediately.

In a sense, you can think of the String between readFile and writeFile as a pipe linking 
the two. Data goes in one end, and flows back out from the other.


