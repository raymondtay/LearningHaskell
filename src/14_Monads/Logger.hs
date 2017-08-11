module Logger( Logger, Log, runLogger, record ) where

type Log = [String] 

runLogger :: Logger a -> (a, Log)
runLogger = undefined

record :: String -> Logger ()

globToRegex cs = globToRegex' cs >>= \ds -> return ('^': ds)


