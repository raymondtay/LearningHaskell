
Althought using `liftM3` tidies up our code, we cannot use a liftM-family
function to solve this sort of problem in general, because the standard
libraries define them only up to `liftM5`. We could write variants up to
whatever number we pleased, but that would amount to drudgery.

If we had a constructor or pure function that takes, say, 10 parameters and
decided to stick with the standard libraries, you might think we would be out
of luck.


