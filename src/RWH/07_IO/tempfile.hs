
import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(catch, finally)

-- the main entry point. Work with a temp file in myAction.

main :: IO ()
main = withTempFile "data.txt" myAction

{- 
    The guts of the program. Called with the path and handle of a temporary
    file. When this function exists, that file will be closed and deleted
    because myAction was called from withTempFile 
-}

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
    do
	        putStrLn "Welcome to tempfile.hs"
	        putStrLn $ "I have a temporary file at " ++ tempname
	        -- let's see what the initial position is 
	        pos <- hTell temph
	        putStrLn $ "My initial position is " ++ show pos
	        -- Now, weite some data to the temporary file
	        let tempdata = show [1..10]
	        putStrLn $ "Writing one line containing " ++ 
	                   show (length tempdata) ++ " bytes." ++ tempdata
	        hPutStrLn temph tempdata
	        -- Get our new position. This doesn't actually modify pos
	        -- in memory, but makes the name "pos" correspond to a different
	        -- value for the remainder for the "do" block.
	        pos <- hTell temph
	        -- Seek to the begining of the file and display it.
	        putStrLn $ "After writing, my new position is " ++ show pos
	        putStrLn $ "The file content is : " 
	        hSeek temph AbsoluteSeek 0
	        -- hGetContents performs a lazy read of the entire file. 
	        c <- hGetContents temph
	        -- Copy the file byte-for-byte to stdout followed by \n
	        putStrLn c
	        
	        -- Let's also display it as a haskell literal
	        putStrLn $ "Which could be expressed as this Haskell literal : "
	        print c

{- this function takes two parameters : a filename pattern and another
    function. it will create a temporary file, and pass the name and 
    Handle of that file to the given function.
    The temporary file is created with openTempFile. The directory is the 
    one indicated by getTemporaryDirectory, or, if the system has no notion of
    a temporary directory, "." is used. The given pattern is passed to openTempFile.

    After the given function terminates,even if it terminates due to tan 
    exception, the handle is closed and the file is deleted.  -}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = 
    do 
        tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
        (tempfile, temph) <- openTempFile tempdir pattern
        finally (func tempfile temph) 
                (do hClose temph 
                    removeFile tempfile)


