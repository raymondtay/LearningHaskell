## About Typeclasses in Haskell

Typeclasses defines a set of functions that can have different implementations
depending on the type of data they are given. Typeclasses may look like the 
objects of object-oriented programming but they are truly quite different.

### When to use the OverlappingInstances extension

According to the book:
<pre>
Here's an important point: GHC treats OverlappingInstances as affecting
the declaration of an instance, not a location where we use the instance.
In other words, when we define an instance we wish to allow
to overlap with another instance, we must enable the extension for the
module that contains the definition. When it compiles the module,
GHC will record that instance as "can beoverlapped with other instances"

Once we import this module and use the instance, we won't need to
enable OverlappingInstances in the importing module. GHC will already
know that the instance was marked as "Ok to overlap" when it was defined. 

This behavior is useful when we are writing a library: we can choose to 
create overlappable instances, but users of our library do not need to
enable any special language extensions.
</pre>

## Understanding and implementing Type classes in Haskell

The following passage is lifted from [Implementing and Understanding Type Classes](http://okmij.org/ftp/Computation/typeclass.html)

Computational abstractions - higher-order functions, continuations, modules, processes and automatic memory 
management have made programs much faster to write, easier to demonstrate correctness and improve code reusability.
And yet there is often subconscious resistence to abstractions: they appear ritualistic, formal -- too abstract. 
One gets the feeling of getting lost. To overcome the mistrust for an abstraction it may help to look at its
realization, to see what is being abstracted away. The awareness of low-level implementation details brings 
the appreciation of an abstraction and the intuitive explanation for it.

Now we look at behind the scenes of the abstraction of parameteric overloading a.k.a bounded polymorphism or just
"type classes". Seeing the implementation makes type classes appear simpler, friendlier, 
and more comfortable ot use. The types and type class definitions ar no longer incantations to memorize: they 
suddenly make sense. Knowing what tedious job GHC is doing for us helps us appreciate more the 
convenience of type classes. 

Dictionary passing, although best known, is not the only compilation strategy for type classes. Historically, first
were static specialization and run-time resolutions, introduced by the father of parameter overloading, Stefan Kaes.
He presented the type system and proved its soundess, descirbe the type inference algorithm na dproved the soundness
and consistency of the two implementation of what is now known as type classes. IT is shameful that his name is almost 
forgotten, his strategies are still in wide use howevr. Local type classes and instances introduced in his paper still
await recognition. 

Dictionary passing makes it easier, in retrospect, to under the other two implementation strategies. Therefore, we
descrieb it first and in detail.WE explain by example, juxtaposing Haskell code with the 
corresponding code in a language with no type classes (OCaml). The implementation language could be any other 
higher-order language, including the GHC COre. For the sake of explanation, we restrict ourselves to 
the single-parameter, non-constructor type classes such as "Num", "Eq" and "Show". The other two implementation
strategies are presented next, illustrating the algorithms from the Kaes's paper, in modern terms.

