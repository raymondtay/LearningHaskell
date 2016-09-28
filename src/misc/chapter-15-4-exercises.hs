module Chapter15_4 where

import Data.Monoid

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mappend msa msa' = Mem $ \_s -> (mappend (fst (runMem msa $ _s)) (fst (runMem msa' $ _s)), _s)

