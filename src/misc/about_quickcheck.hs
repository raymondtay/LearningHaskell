
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity a = (a <> mempty) == a
monoidRightIdentity a = (mempty <> a) == a

-- The quickCheck function uses the arbitrary typeclass to provide
-- the randomly generated inputs for testing the function. Although
-- it's common to do so, we may not want to rely on an Arbitrary instance
-- existing for the type of our inputs. We may not want to do this
-- for one of a few reasons.
-- It may be that we need a generator for a type that doesn't belong to us,
-- so we would rather not make an orphan instance. Or it could be a type that
-- already has an arbitrary instance, but we want to run tests with a different
-- random distribution of values, or to make sure we check certain special edge
-- cases in addition to random values.
--
--
-- quickCheck monoidAssoc
-- verboseCheck monoidAssoc
