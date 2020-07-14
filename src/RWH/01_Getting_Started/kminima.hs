-- Lazy evaluation has some spooky effects. 
-- Let's assume we want to find the k least-valued
-- elements of an unsorted list. 
-- For efficiency reasons, we would write a fn that takes 
-- these values in one pass, and that would have to 
-- perform some moderately complex bookkeeping. 
-- In haskell, the sort-then-take approach works well
-- laziness ensures that the list will only be sorted
-- enough to find the k minimal elements

minima k xs = take k (sort xs)

