
--
-- Demo of the MonadReader
--

import Control.Monad.Reader

addOne :: Reader Int Int
addOne = do i <- ask
            return (i+1)

addTwo :: Reader Int Int
addTwo = do i <- ask
            local (+1) addOne -- modify the environment, addOne is run against the result of the "local" op.

mulTwoPlusOne :: Reader Int Int
mulTwoPlusOne = do i <- ask
                   local (*2) addOne -- modify the environment, addOne is run against the result of the "local" op.

-- Run the following like this:
-- > runReader addOne 4
-- > 5
-- > runReader addTwo 4
-- > 6
-- > runReader mulTwoPlusOne 2
-- > 5


