
-- Haskell programmers use hGetContents as a filter quite often. 
-- They read from one file, do something to the data, and write 
-- the result out elsewhere.
-- This is so common that there are some shortcuts for doing it.
-- readFile and writeFile are shortcuts for working with files 
-- as strings. They handle all the details of opening files, closing
-- files, reading data and writing data. readFile uses hGetContents internally.

import Data.Char(toUpper)

main = do
    inputString <- readFile "input.txt"
    writeFile "output.txt" (map toUpper inputString)
