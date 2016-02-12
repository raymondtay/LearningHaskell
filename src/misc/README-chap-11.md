# What makes these data types algebraic ?

Algebraic data types in HAskell are algebraic because we can describe the
patterns of argument structures using two basic operations: sum and product.
The most direct way to explain why they're called sum and product is to demonstrate
sum and product in terms of cardinality. This can be understood in terms of the
cardinality you see with finite sets. 

This doesn't map perfectly as we can have infinite data structures in Haskell, 
but it's a good way to begin understanding and appreciating how data types work. When
it comes to programming languages we are concerned with computable functions, 
but not just those which can generate a set.

