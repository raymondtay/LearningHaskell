{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import qualified Supply as S

class (Monad m) => MonadSupply s m | m -> s where
  next :: m (Maybe s)

--instance MonadSupply s (S.Supply s) where
  --next = S.next
{-
  Multiparameter Typeclasses


  `| m -> s` is a functional dependency, often called a fundep.
  We can read the vertical bar | as "such that" and the arrow -> 
  as "uniquely determines". Our functional dependency establishes
  a relationship between 'm' and 's'.

  The FunctionalDependencies language pragma governs the availability
  of functional dependencies. The purpose behind us declaring a 
  relationship is to help the type checker. Recall that a type checker
  is essentially a theorem prover, and that its conservative in how
  it operates:

-}
