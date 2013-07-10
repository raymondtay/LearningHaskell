-- isolation is the keyword to understand

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
        input <- readFile inputFile
        writeFile outputFile (function input)
-- the 'do' keyword introduces a block of actions
-- that can cause effects in the real world, such as reading 
-- or writing a file. The <- operator is the equivalent of assignment
-- inside a do-block. This is enough explanation to get us 
-- started. 
main = mainWith myFunction
    where   myFunction = id 
            mainWith function = do
	        args <- getArgs
	        case args of 
	            [input, output] -> interactWith function input output
	            _               -> putStrLn "error: exactly two arguments needed"


