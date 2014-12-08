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

