-- | The `Chan` type is implemented using `MVars`. Use `MVars` to develop a
-- `BoundedChan` library.
-- Your `newBoundedChan` function should accept an Int parameter, limiting the
-- number of unread items that can be present in a `BoundedChan` at once. If
-- this limit is hit, a call to your `writeBoundedChan` function must block
-- until a reader uses readBoundedChan to consume a value.
--

import Control.Concurrent

