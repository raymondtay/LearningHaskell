import PhantomT

main :: IO ()
main = do
  case validate . formData $ "Give it to me" of
    Just s -> useData(s)
    Nothing -> putStrLn "Nothing"
  case validate . formData $ "Nooo" of
    Just s -> useData(s)
    Nothing -> putStrLn "Nothing"
  case validate . formData $ "GIVE IT TO ME" of
    Just s -> useData(s)
    Nothing -> putStrLn "Nothing"
  useData $ sanitise . formData $ "GIVE IT TO ME"
  useData $ sanitise . formData $ "give it to me!!"





