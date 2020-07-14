{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module SupplyClass (
  MonadSupply(..),
  S.runSupply,
  S.Supply) where 

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

  The FlexibleInstances extension is necessary so that the compiler will accept 
  our instance declaration. This extension relaxes the normal rules for writing 
  instances in some circumstances, in a way that still lets the compiler's type checker
  guarantee that it will terminate.

  Finally, notice that we are re-exporting the `runSupply` and `Supply` names from this module,
  its perfectly legal to export a name from one module even though it's defined in another. 
  In our case, it means that client code needs only to import the `SupplyClass` module, without 
  also importing the `Supply` module. This reduces the number of "moving parts" that a user
  of our code needs to keep in mind.

-}
