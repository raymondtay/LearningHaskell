import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import Control.Exception (catch, finally)

-- random musings on generic type parameters...
-- so what happened was that i came across the following definition
-- and was wondering how readable it was .... but the next thing i wondered was
-- whether it was compilable ?
-- i mean, just because i read it from the docs doesn't necessarily mean it
-- actually compiles right? 
--
-- So next thing was to test it out and it turns out what i needed to do was
-- simply as how i've defined in a test. If i were to run this in ghci, then i
-- need to set the compiler option "set -XExistentialQuantification"
--
-- ```haskell
-- {-# LANGUAGE ExistentialQuantification #-}
-- import Control.Exception
--
-- data SomeFrontEndException = forall e . Exception e => SomeFrontEndException e
--
-- ```
--


-- The main entry point. Work with a temp file in myAction
main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
  do
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file at " ++ tempname
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++ show (length tempdata) ++ " bytes: " ++ tempdata
    hPutStrLn temph tempdata

    pos <- hTell temph
    putStrLn $ "After writing, my new position is " ++ show pos

    putStrLn $ "The file content is: "
    hSeek temph AbsoluteSeek 0
    c <- hGetContents temph
    putStrLn c
    putStrLn $ "Which could be expressed as this haskell literal:"
    print c


withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = 
  do
    tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph)
            (do hClose temph
                removeFile tempfile)

