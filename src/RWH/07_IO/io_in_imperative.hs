
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
    input <- openFile "test_input.txt" ReadMode
    output <- openFile "test_output.txt" WriteMode
    mainloop input output
    hClose input
    hClose output

--
-- :t hIsEOF :: Handle -> IO Bool
-- :t hPutStrLn :: Handle -> String -> IO ()
--
-- mainloop begins by checking to see if we are at the end of file for the input.
-- If not, we read a line from the input. We write out the same line to the output
-- after first converting it to uppercase. Then we recursively call mainloop again to continue 
-- processing the file.
--
mainloop :: Handle -> Handle -> IO ()
mainloop inputH outputH = 
    do ineof <- hIsEOF inputH
       if ineof 
       then return ()
       else do inputString <- hGetLine inputH
               hPutStrLn outputH (map toUpper inputString)
               mainloop inputH outputH

-- Notice the "return" call. This is not really the same as return in C or Python.
-- In those languages, the "return" is used to terminate execution of the current function
-- immediatley,and to return a value to the caller. In Haskell, "return" is the opposite of "<-".
-- That is, "return" takes a pure value and wraps it inside IO. Since every I/O action must
-- return some UI type, if your result came from pure computation, you must use return to wrap it in an IO.

