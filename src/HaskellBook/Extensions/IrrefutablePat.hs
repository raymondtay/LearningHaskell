
{-
  Lazy or irrefutable, patterns (denoted with the syntax ~pat) are patterns that always match, without even looking at the matched value. This means lazy patterns will match even bottom values. However, subsequent uses of variables bound in sub-patterns of an irrefutable pattern will force the pattern matching to occur, evaluating to bottom unless the match succeeds.     
-}
f1 :: Either e Int -> Int
f1 ~(Right 1) = 42

f2 :: Either e Int -> Int
f2 ~(Right x) = x + 1

main :: IO ()
main = do
  putStrLn . show $ f1 (Right 1)
  putStrLn . show $ f1 (Right 2)
  putStrLn . show $ f1 (Left "Ops")
  putStrLn . show $ f1 (error "Oh no!")

  putStrLn . show $ f2 (Right 1)
  putStrLn . show $ f2 (Right 2)
  putStrLn . show $ f2 (Left "Ops") -- as 'x' in the pattern is being used, the evaluation will be eager
  putStrLn . show $ f2 (error "Oh no!")
