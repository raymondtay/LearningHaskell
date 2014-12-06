
import Data.Text (strip)
-- This is a simple example of 'read' and 'show' together. Notice that
-- we gave an explicit type of 'Double' when processing the 'read'. That's because
-- 'read' returns a value of type 'Read a => a' and 'show' expects a value of type 'Show a => a'
-- There aer mahy types that have instances defined for both 'Read' and 'Show. w/o knowing a specific 
-- type, the compiler must guess from these many types which one is needed. In situations such as
-- this, it mayoften choose Integer.

main = do
    putStrLn "Please enter a double.."
    d <- getLine
    let ddata = (read d) :: Double
    putStrLn ("Twice " ++ show ddata ++ " is " ++ show (ddata * 2))


data Color = Red | Green | White | Blue deriving (Show, Eq)


instance Read Color where
    readsPrec _ value = 
        tryParse [("Red", Red), ("White", White), ("Green", Green), ("Blue", Blue)]
        where tryParse [] = []
              tryParse ((attempt, result) : xs) = 
                if (take (length attempt) (strip value)) == attempt
                    then [(result, drop (length attempt) (strip value))]
                    else tryParse xs

