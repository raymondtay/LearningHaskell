
import System.IO
import System.Directory (getTemporaryDirectory, removeFile) 
import System.IO.Error (catch) -- `catch` was removed in the recent version of GHCi (i.e. 8.0.1); not sure when exactly was it removed.
import Control.Exception (finally)

-- Main entry point. Work with a temp file in myAction.
--
main :: IO ()
main = withTempFile "my_temp.txt" myAction

{- The guts of the program. Called with the path and handle of a temporary
    file. when this function exist, that file will be closed and deleted because 
    myAction was called from withTempFile. -}

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
  do -- start by displaying a greeting on the terminal
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file at " ++ tempname

    -- Let's see what the initial position is.
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos

    -- Now, write some data to the temporary file
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++ show (length tempdata) ++ " bytes: " ++ tempdata
    hPutStrLn temph tempdata

    -- Get our new position. This doesn't actually modify pos in memory, but
    -- makes the name "pos" correspond to a different value for the remainder
    -- of the "do" block.
    pos <- hTell temph
    putStrLn $ "After writing, my new position is " ++ show pos

    -- Seek to the beginning of the file and display it
    putStrLn $ "The file content is : " 
    hSeek temph AbsoluteSeek 0

    -- hGetContents performs a lazy read of the entire file
    c <- hGetContents temph

    -- Copy the file byte-for-byte to stdout, followed by \n
    putStrLn c

    -- Let's also display it as a Haskell literal
    putStrLn $ "Which could be expressed as this Haskell literal: " 
    print c

{- 
  This function takes two parameters: a filename pattern and another funciton. IT will create a temprary file, 
  and pass the name and handle of that file to the given function.

  The temporary file is created with openTempFile. The directory is the one indicated by the
  getTemporaryDirectory, or , if the system has no notion of a temporary directory, "." is used.
  The given pattern is passed to openTempFile.

  After the given function terminates, even if it terminates due to an exception, the Handle
  is closed and the file is deleted. -}


