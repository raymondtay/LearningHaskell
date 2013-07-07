import qualified Data.Text as T

main = interact wordCount
    where wordCount input = "length = " ++ show (length (lines input)) ++ ",words in file = " ++ show (wordsInFile (lines input)) ++ ",characters in file = " ++ show (charsInFile (lines input)) ++ "\n"
          wordsInFile [] = 0
          wordsInFile (x:xs) = (length (words x)) + wordsInFile xs
          charsInFile [] = 0
          charsInFile (x:xs) = let t = T.pack(x) in
                                    T.length(t) + charsInFile xs

