{-# LANGUAGE InstanceSigs #-}

import Control.DeepSeq

data Tree a = Empty | Branch (Tree a) a (Tree a) 

-- GHC will force you to add the constraint to make sure every part of a Tree
-- is "reducible" to normal-form.
--
instance NFData a => NFData (Tree a) where
  rnf :: Tree a -> ()
  rnf Empty = ()
  rnf (Branch l v r) = rnf l `seq` rnf v `seq` rnf r

