# Parser Combinators

The word "parse" comes from the Latin word for "parts", and means to analyze a
sentence and label the syntactic role, or part of speech of each component.
Language teachers once emphasized this ability because it forced students to think
closely about the structure of sentences, the relationships among the parts, and the
connection between the structure and the meaning of the whole. Diagramming sentences wa 
was also common because it made parsing visual and somewhat concrete. It is now
common to represent grammatical structures of natural languages as trees.

Parsing is a huge field of research in its own right with connections that span natural
language processing, linguistics, and programming language theory. The underlying types
and typeclasses of the libraries we'll be using are complicated. To be sure, if you enjoy
parsing and expecting to do it a lot, those are things you would want to learn; they are
simply out of the scope of this book.

A parser is a function that takes some textual input (it could be a String in Haskell,
or another datatype such as ByteString or Text) and returns some structure as an output.
That structure might be a tree, for example, or an indexed map of locations in the parsed
data. Parsers analyze structure in conformance with rules specified in a grammer, whether it's
a grammer of a human language, a programming language, or a format such as JSON.

A parser combinator is a higher-order function that takes parsers as input and returns a new
parser as output. You may remember our brief discussion of combinators way back in the lambda 
calculus chapter. Combinators are expressions with no free variables.

One of the hardest problems in writing parsers, especially the parser libraries themselves
is making it easy to express things the way the programmer would like, but still have the resulting
parser be fast.

