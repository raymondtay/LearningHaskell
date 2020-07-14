
-- Multiparameter Typeclasses
--
-- How should we read the snippet MonadSupply s m in the typeclass? if we add
-- parentheses an equivalent expression is (MonadSupply s) m, which is a little
-- clearer. In other words, given some type variables m that is a Monad, we can
-- make it an instance of the typeclass MonadSupply s. Unlike a regular
-- typeclass, this one has a parameter. 
--
-- AS this language extendsion allows a typeclass to have more than one
-- parmaeter, its name is MultiParamTypeclasses. The parameter s servers the
-- 
--
-- Functional Dependencies
--
-- To revisit a snippet that we ignored earlier, | m -> s is a functional
-- dependency, often calledf a fundep. We can read the veritcal bar as such
-- that and the arrow -> as uniquely determines. Our fundep establishes a
-- relationship between m and s.
--
-- The functionalDependencies language pragma governs thje availability of
-- functional dfependencies. The purpose behinmd us declaring relationship is
-- to hlep the type checker. Recall that a ahaskell tyupe checker is essentiall
-- y a theorem prover, and that is conservative in how it operates: it insists
-- that its proofs must terminate. A non terminating proof results in the
-- compiler either giving up or getting stuck in an infinite loop.
--
-- Without our functional dependency, we are telling the tyep checker that
-- every time it sees some monad m being used in the context of a MonadSupply s
-- the type s is the only acceptable type to use with it. If we were to omit
-- the functional dependency the tyep checker would simply give up with an
-- error message. 
--
-- It is hard to picture what the relationshp bteween m and s really means oso
-- let us look at an instance of this typeclass:
--

import qualified Supply as S

instance MonadSupply s (S.Supply s) where
  next = S.next


