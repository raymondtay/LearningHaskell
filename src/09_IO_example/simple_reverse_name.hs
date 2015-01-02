{-

Raymonds-MacBook-Air:09_IO_example tayboonl$ ghc --make ./simple_reverse_name.hs
[1 of 1] Compiling Main             ( simple_reverse_name.hs, simple_reverse_name.o )
Linking simple_reverse_name ...
Raymonds-MacBook-Air:09_IO_example tayboonl$ ./simple_reverse_name
raymond tay
dnomyar yat
'Hit ctrl-c' simple_reverse_name: <stdin>: hGetLine: end of file

-}

main = do
    line <- getLine
    if null line
    then return ()
    else do
        putStrLn $ reverseWords line
        main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

