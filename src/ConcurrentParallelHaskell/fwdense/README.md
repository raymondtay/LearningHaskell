
The function argument used with `foldP` must be _associative_. That is the
function f must satisfy f x (f y z) == f (f x y) z . This is because unlike
foldS, foldP does not necessarily fold the function over the array elements in
strict left-to-right order; it folds different parts of the array in parallel
and then combines the results from those parts using the folding function.

Note that strictly speaking, although mathematical addition is associative,
floating point addition is not , due to rounding errors. However, we tend to
ignore this detail when using foldP because a small amount of nondeterminism in
the floating point result is normally acceptable.
