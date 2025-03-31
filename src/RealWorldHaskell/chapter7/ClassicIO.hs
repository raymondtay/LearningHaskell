module ClassicIO (
  name2reply,
  doUpper
) where

-- When we speak of pure code, we are talking about Haskell functions that
-- always return teh same result when given the same input and have no side
-- effects.
-- In Haskell, only the execution of I/O actions avoid these rules.
--
-- -------------------------------------------------|----------------------------------------------------------------
-- | <<Pure>>                                       | <<Impure>>                                                    |
-- | -----------------------------------------------| ---------------------------------------------------------------
-- | Always prduces the same result when given the same params. | May produce different results for the same params.|
-- | -----------------------------------------------| ---------------------------------------------------------------
-- | Never has side effects.                        | May have side effects.                                        |
-- | -----------------------------------------------| ---------------------------------------------------------------
-- | Never alters state.                            | May alter the global state of the program, system or world.   |
-- | -----------------------------------------------| ---------------------------------------------------------------
--

import           Data.Char (toUpper)
import           System.IO

name2reply :: String -> String
name2reply name =
  "Pleased to meet you, " ++ name ++ ".\n" ++
  "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

invokeName2reply :: IO ()
invokeName2reply = do
  putStrLn "Greetings once again. What is your name? "
  inputStr <- getLine
  putStrLn $ name2reply inputStr

doUpper :: IO ()
doUpper = do
  inH <- openFile "./input.txt" ReadMode
  outH <- openFile "./output.txt" WriteMode
  mainloop inH outH
  hClose inH
  hClose outH

mainloop :: Handle -> Handle -> IO ()
mainloop inH outH =
  do ineof <- hIsEOF inH
     if ineof then return () -- return is the opposite of <-. That is, return takes a pure value and wraps it inside IO.
              else do inputStr <- hGetLine inH
                      hPutStrLn outH (map toUpper inputStr)
                      mainloop inH outH


