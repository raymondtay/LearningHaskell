
# Coherency in Haskell

Lifted from the page: http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/

Confluence is a property that comes from term-rewriting: A set of instances is
confluent, if no matter what order constraint solving is performed, GHC will
terminate with a canonical set of constraints that must be satisfied for any
given use of a typeclass. In other words, confluence says that we won't
conclude that a program doesn't type check just because we swapped it in a
different constraint solving algorithm.

Confluence's closely related twin is coherence. This propertuy states that
every different valid typing derivation of a program leads to a resulting
program that has the same dynamic semantics. Why could differing typing
derivations result in different dynamic semantics? The answer is that context
reduction, which picks out type class instances, elaborates into concrete
choices of dictionaries in the generated code. Confluence is a prerequisite for
coherence, since one can hardly talk about the dynamic semantics of a program
that doesn't type check.

So, what is it that people often refer to when they compare Scala type classes
to Haskell type classes? I am going to refer to this as global uniqueness of
instances, definiing to say: in a fully compiled program, for any type, there
is at most one instance resolution for a given type class. Languages with local
type class instances such as Scala generally do not have this property, and
this assumption is a very convenient one when building abstractions like sets.

So, what properties does GHC enforce, in practice? In the absense of any
typesystem extensions, GHC exmploys a set of rules to ensure that type class
resolution is confluent and coherent. Intuitively, it achieves this b y having
a very simple constraint solving algorithm and then requireing the set of
instances to be nonoverlapping, ensuring there is onlhy every one way to solve
a wanted constraint. Overlap is a more stringent restriction than either
confluence or coherence, and via the OverlappingInstances and
IncoherentInstances, GHC allows a user to relax this restriction "if they know
what they are doing."

