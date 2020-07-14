
import System.IO
import Data.Char(toUpper)

-- Notice that the hGetContents handled all of the readings for us. Also, 
-- take a look at processData. It is a pure funciotn since it has no side effects and 
-- always return the same result ewach time it is called. It has no need to know and no way 
-- to tell tha tits input is being read lazily from a file in this case. It can work
-- perfectly well witha 20-character literal or a 500GB data dump on disk

main :: IO ()
main = do
    input <- openFile "test_input.txt" ReadMode
    output <- openFile "test_output.txt" WriteMode
    inputData <- hGetContents input
    let result = processData inputData
    hPutStr output result
    hClose input
    hClose output

processData :: String -> String
processData = map toUpper

-- If we had tried to hang on to inputData in the example 
-- past the one place where it was used (i.e. the argument passed to processData)
-- then the program would have lost its memory efficiency. That's because
-- the compiler would have been forced to keep inputData's value in memory
-- for future use. Here it knows that inputData will never be reused
-- and frees the memory as soon as it is done with.

