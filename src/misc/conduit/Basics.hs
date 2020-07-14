
import Conduit

main = do
  -- Pure operations : summing numbers.
  print $ runConduitPure $ yieldMany [1..10] .| sumC
  -- Exception safe file access : copy a file.
  writeFile "input.txt" "This is a test."
  runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt"
  readFile "output.txt" >>= putStrLn
  -- Perform transformations.
  print $ runConduitPure $ yieldMany [1..10] .| mapC (+1) .| sinkList

