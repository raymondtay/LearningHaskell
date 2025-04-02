module TempFile (
  doTempFile
) where

import           Control.Exception (finally)
import           System.Directory  (getTemporaryDirectory, removeFile)
import           System.IO
import           System.IO.Error   (catchIOError)


doTempFile :: FilePath -> IO ()
doTempFile tmpFile = withTempFile tmpFile action

{-
    The guts of the program. Called with the path and handle of a temporary
    file. When this function exits, that file will be closed and deleted
    because action was called from withTempFiel.
-}
action :: FilePath -> Handle -> IO ()
action tempname temph =
  do -- Start by displaying a greeting on the terminal
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file at: " ++ tempname

    -- See what the initial position is.
    pos <- hTell temph
    putStrLn $ "My initial position is: " ++ show pos

    -- now, write some data to the temporary file
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++
               show (length tempdata) ++ " bytes: " ++
               tempdata
    hPutStrLn temph tempdata

    -- Get our new position. This does not actually modify pos in memory,
    -- but makes the name "pos" correspond to a different value for the
    -- remainder of the "do" block.
    init_pos <- hTell temph
    putStrLn $ "After writing, my new position is: " ++ show init_pos

    -- Seek to the beginning of the file and display it
    putStrLn $ "The file content is: "
    hSeek temph AbsoluteSeek 0
    -- hGetContents performs a lazy read of the entire file
    c <- hGetContents temph

    -- Copy the file byte-for-byte to stdout, followed by \n
    putStrLn c

    -- Let's also display it as a Haskell literal
    putStrLn $ "Which could be expressed as this Haskell literal: "
    print c


{-
   This function takes two parameters: a filename pattern and another function.
   It iwll create a temporary file, and pass the name and Handle of that
   file to the given function.

   The temporary file is created with openTempFile. The directory is the one
   indicated by getTemporaryDirectory, or, if the system has no notion of a
   temporary directory, "." is used. The given pattern is passed to
   openTempFile.
-}
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
  do
    tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern

    finally (func tempfile temph)
            (do hClose temph
                removeFile tempfile)

