
## About bytestrings in haskell
Lists are a cool and useful data structure. So far, we've used them pretty much everywhere.
There are a multitude of functions that operate on them and Haskell's laziness allows us to 
exchange the for and while loops of other languages for filtering and mapping over lists, 
because evluation will only happen once it really needs to, so things like infinite lists (and even
infinite lists of infinite lists) are no problem for us. That's why lists can also be used to 
represent streams, either when reading form the standard input or when reading from files. We can 
just open a file and read it as a string even though it will only be accessed when the need arises.

However, processing files as strings has 1 drawback: It tends to be slow. 
The reason for their slowness is [TODO]

ByteStrings comes in two flavors: strict and lazy (no kidding). They are organized and identified by
their module names "Data.ByteString" and "Data.ByteString.Lazy". There are no promises involved, a strict bytetring
represents a series of bytes in an array. You cannot have things like infinite strict bytestrings. If you 
evaluate the first byte of a strict bytestring, you have to evaluate it whole. The pro is that 
there might be less overhead as no thunks are present but the con is that this mechanism hydrates your memory
far faster than the previous.

The other is "Data.ByteString.Lazy" stores data into chunks of sizes 64K (noticed the strong correlation
behind that number and the size of a CPU cache ;-) This different approach operates like a linked list
where each element of that list is of 64K and needless to say that this way isn't quite good.

