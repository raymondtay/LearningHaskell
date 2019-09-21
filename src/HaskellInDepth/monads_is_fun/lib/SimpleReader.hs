import qualified Data.Map as Map
import Control.Monad.Reader

type Bindings = Map.Map String Int

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
    count1 <- asks (lookupVar "count")
    count2 <- asks (lookupVar "b")
    bindings <- ask
    return $ (&&) (count1 == (Map.size bindings)) (count2 == count2)

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = maybe 0 id (Map.lookup name bindings)

-- mapReader is handy when the result of a reader-run might need to be
-- transformed to another representation
convertIt :: Reader Bindings Integer
convertIt = mapReader (\boolValue ->
  case boolValue of
    True -> 1
    False -> 0
  ) calc_isCountCorrect

-- withReader is handy when it comes to modifying the environment whereas
-- mapReader is useful when it comes to modifying the result
convertReader :: Reader Bindings Bool
convertReader = withReader (\r -> Map.fromList [("count",0), ("b",1)]) calc_isCountCorrect

sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]

calculateContentLen :: Reader String Int
calculateContentLen = do
    content <- ask
    return (length content);

-- Calls calculateContentLen after adding a prefix to the Reader content.
calculateModifiedContentLen :: Reader String Int
calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen

-- The Reader/IO combined monad, where Reader stores a string.
printReaderContent :: ReaderT String IO ()
printReaderContent = do
    content <- ask
    liftIO $ putStrLn ("The Reader Content: " ++ content)

main = do
    runReaderT printReaderContent "Some Content"
    let s = "12345";
    let modifiedLen = runReader calculateModifiedContentLen s
    let len = runReader calculateContentLen s
    putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
    putStrLn $ "Original 's' length: " ++ (show len)
    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": "
    putStrLn $ show (isCountCorrect sampleBindings)


