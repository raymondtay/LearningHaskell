
Althought using `liftM3` tidies up our code, we cannot use a liftM-family
function to solve this sort of problem in general, because the standard
libraries define them only up to `liftM5`. We could write variants up to
whatever number we pleased, but that would amount to drudgery.

If we had a constructor or pure function that takes, say, 10 parameters and
decided to stick with the standard libraries, you might think we would be out
of luck.

Together with the haskell language extension `GeneralizedNewtypeDeriving`, we
can basically remove boilerplate code from the implementation. See
[[Supply.hs]] for an example of how this can be done.

Another important way to make code more modular involves separating its
interface (what the code can do) from its implementation - how it does it.

The standard random number generator in `System.Random` is known to be quite
slow. If we use our `randomsIO` function to provide it with random numbers,
then our next action will not perform well.

One simple and effective way that we could deal with this is to provide
`Supply` with a better source of random numbers. Let's set this idea aside,
though, and consider an alternative approach, one that is useful in many
settings. We will separate the actions we can perform with the monad from how
it works using a typeclass (See [[SupplyClass.hs]])

# Using Typeclasses

On page 378, the passage reads 
"""
The disadvantage of hiding IO in another monad is that we are still tied to a
concrete implementation. If we want to swap HandleIO for some other monad, we
must change the type of every action that uses HandleIO.

"""

**Note** : Go read the source files [[MonadHandle.hs]], [[MonadHandleIO.hs]], 


# The Writer Monad and Lists

Page 380 of the book reads:
"""

The Writer Monad uses the Monoid's mappend function every time we use `tell`.
Because `mappend` for lists is `(++)`, lists are not a good practical choice
for use with `Writer` : repeated appends are expensive. We use lists previously
purely for simplicity. 

In production code, if you want to use the `Writer` monad and you need
list-like behavior, use a type with better append characteristics. One such
type is the difference list, which we introduced on page 317. You don't need to
roll your own difference list implementation: a well-tuned library is available
for download from Hackage, the haskell package database. Alternatively, you can
use the `Seq` type from `Data.Sequence` module.
"""
