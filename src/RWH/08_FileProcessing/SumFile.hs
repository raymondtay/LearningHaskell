{-
	Although the String type is the default used for reading and writing files, it is not
	efficient, so a simple program like this will perform badly.
	
	A String is represented as a list of Char values; each element of a list is allocated
	individually and has some bookkeeping overhead. These factors affect the memory 
	consumption and performance of a program that must read or write text or binary data.
	
	On Simple benchmarks like this, even programs written in interpreted languages such 
	as Python can perform Haskell code that uses String by an order of magnitude.
	
	The bytestring library provides a fast, cheap alternative to the String type. Code
	written with bytestring can often match or exceed the performance and memory
	footprint of C, while maintaining Haskell's expressivity and conciseness.

    Data.ByteString - strict type that represents a string of binary or text data in a single array
    Data.ByteString.Lazy - lazy type which is a string of data as a list of chunks, arrays of up to 64KB in size
	
-}

main = do
    contents <- getContents
    print (sumFile contents)
    where sumFile = sum . map read . words

