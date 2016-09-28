module Chapter15_4 where

import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s) -- Haskell understands from the type signature that 'a' is actually a Monoid which explains why it allows the expression 'mempty' 
  mappend msa msa' = Mem $ \_s -> (mappend (fst (runMem msa $ _s)) (fst (runMem msa' $ _s)), _s)

f' = Mem $ \s -> ("hi", s + 1)

main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0

