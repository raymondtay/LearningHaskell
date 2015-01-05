
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO())]
dispatch = [("add", add), ("view", view), ("remove", remove)]

{-
once the program is made via "ghc --make simple_n_stupid.hs" , you would
obtain a new program named "simple_n_stupid"
and here's how you would add/view/ and remove something
1) To add, you invoke "./<prog> add data.txt XXX"
2) To view, you invoke "./<prog> view data.txt"
   which returns a list of items (separated by newlines) and each item is prefixed by a number
3) To remove, you invoke "./<prog> remove data.txt <num>" where u replace <num> with the number from (2)
-}

add :: [String] -> IO ()
add [filename , todo] = appendFile filename (todo ++ "\n")

view :: [String] -> IO ()
view [filename] = do
    contents <- readFile filename
    let todos = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todos
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [filename, nS] = do
    h <- openFile filename ReadMode
    (tName, tH) <- openTempFile "." "temp"
    c <- hGetContents h
    let num = read nS
        todo = lines c
        newTodos = delete (todo !! num) todo
    hPutStr tH $ unlines newTodos
    hClose h
    hClose tH
    removeFile filename
    renameFile tName filename

main = do 
    (command : args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
