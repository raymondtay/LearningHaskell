
module VCard where

import Control.Monad

data Context = Home | Mobile | Business deriving (Eq, Show)

type Phone = String

albulena = [(Home, "1212")]

nils = [(Mobile, "124"), (Business, "1312312312")]

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                          Nothing -> lookup Mobile ps
                          Just n -> Just n

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
  where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns

contextIs a (b, _) = a == b -- interesting how a simple function like this is able to perform provide context ;) no pun intended.

lookup2 :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup2 _ [] = Nothing
lookup2 k ((x,y) : xys) | x == k = Just y
  | otherwise = lookup k xys

-- Here is how we can generalize lookup2 function
lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ [] = mzero
lookupM k ((x, y) : xys) | x == k = return y `mplus` lookupM k xys
  | otherwise = lookupM k xys


-- Adventures in hiding the plumbing
--
-- a drawback of the code we developed is that is is leaky: users know that
-- they ar eexecuting inside the State monad. This means that they can inspect
-- and modify the state of the random number generator just as easily as we,
-- the authors, can.
--
-- human nature dictates that if we leave our internal workings exposed,
-- someone iwll surely come along and monkey with them. For a sufficiently
-- small program, this may be fine but in a larger software project, when one
-- consumer of a library modifies its internals in  way that other consumers
-- are not prepared for, the resulting bugs can be among the most diffiuclt to
-- track down. These bugs occur at a level where we are unlikely to question
-- our basic assupmtions about a library until long after we have exhausted all
-- other avenues of inquiry.
--
