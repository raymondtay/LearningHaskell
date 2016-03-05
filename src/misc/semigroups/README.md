
If you do install Semigroups and import the modules, 
keep in mind that it defines its own more general version of 
(<>) which only requires a Semigroup constraint rather than a 
Monoid constraint. If you dont know what to do, 
close your REPL and reopen it. After doing so, 
then import only the semigroups module and not Data.Monoid
from base. If you must import Data.Monoid, hide the 
conflicting (<>) in your import declarations.

When haskellers talk abotu the strength of an algebra, they usually mean
the nujmber of operations it provides which in turns expands what you can do
with any given instance of that algebra without need to know specifically
what type you are working with.

The reason we cannot and do not want to simply make all of our algebras
as big as possible is that there are datatypes which are very useful representationlly
but whcih do not have the ability to satisfy everything in a larger algebra
that could work fine if you removed an operation or law. This becomes a serious
problem if NonEmpty is the right data type for something in the domain you are 
representing. If you are an experienced progarmmer think carefully. How many times
have you meant for a list to never be empty? To guarantee this and make the types
more informative, we use types like NonEmpty.
