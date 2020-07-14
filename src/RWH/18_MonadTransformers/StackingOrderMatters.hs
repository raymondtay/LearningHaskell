
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Writer
import MaybeT

-- Look at [[MaybeT.hs]] for the instance declaration for MonadWriter
--
problem :: MonadWriter [String] m => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"

type A = WriterT [String] Maybe

type B a = MaybeT (Writer [String]) a

a :: A ()
a = problem

b :: B ()
b = problem


-- From page 442:
-- Our WriterT on Maybe stack has Maybe as the underlying monad, so runWriterT
-- must give us back a result of type Maybe. In our test case, we get to see
-- only the log of what happened if nothing actually went wrong!
--
-- Stacking monad transformers is analogous to composing functions. If we
-- change the order in which we apply functions and then get different results,
-- we won't be surprised. So it is with monad transformers, too.
--
--

