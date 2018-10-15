import GHC.Conc (numCapabilities)
import System.Environment (getArgs)

-- if we compile and run this program we can see that theoptions to the runtime
-- system are not visible to the program but we can see how many cores it can
-- run on
-- ghc -c NumCapabilities.hs
-- ghc -threaded -o NumCapabilities NumCapabilities.o
-- ./NumCapabilities +RTS -N4 -RTS foo
--
-- On my machine its a Intel Core i5 and best i have is a Intel Core i7 
-- which holds 4, 8 cores respectively.
--
main = do
  args <- getArgs
  putStrLn $ "command line arguments: " ++ show args
  putStrLn $ "number of cores: " ++ show numCapabilities

