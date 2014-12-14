

## About efficient file processing in Haskell

Quoting the book on page 194, 
<pre>
Qualified imports make it easy to switch between ByteString types. 
All you should need to do is modify an import declaration at the top
of your source file; the rest of your code will probably not need any changes.
You can thus handily benchmark the two types to see whcih is best 
suited to your application needs.

`import qualified Data.ByteString.Lazy as L`

Whether or not we use qualified imports we can always use the entire name of amodule
to identify something unambiguously. Both Data.ByteString.Lazy.length and
L.length for instance identify the same function as do Prelude.sum and sum.
</pre>

### About using regular expressions in Haskell

The book recommends that i start with Text.Regex.Posix which is what i'm going to do.
According to the book,
<pre>
...
The only function that we are likely to need for normal use if the regexp matching function
an infix operator named (=~) which is borrowed from Perl. The first hurdle to overcome is that
Haskell's regexp libraries make heavy use of polymorphism. As a result, the type
signature of the (=~) operator is difficult to understand, so we will not explain it here.

The =~ uses typeclasses for both of its arguments and also for its return type.
The first argument (on the left of the =~) is the text to match; the second (on the right)
is the regular expression to match against. We can pass either a String or a ByteString as argument.
</pre>
No wonder (=~) looked so familiar!

There's a cautionary tale about the differences between the Perl and POSIX regular expressions
Quoting from the book,
<pre>
...
Perl regexp engines perform left-biased matching when matching alternatives whereas POSIX engines choose the
greediest match. What this means is that given a regexp of (foo|fo*) and a text string of foooooo, a Perl-style
engine will give a match of foo (leftmost match) while a POSIX engine will match the entire string since 
its the longest match it could find.
POSIX regexps have less uniform syntax than Perl-style regexps. They also lack a number of capabilities
provided by Perl-style regexps, such as zero-width assertions and control over greedy matching.
</pre>

Haskell does not require that a value or function be declared or defined
in a source file before it's used. It's perfectly normal for a defn to 
come after the first place it's used. 
The compiler doesnt' care about ordering at this level. This grants us the
flexibility to structure our code in the manner that makes more logical sense
to us, rather than follow an order that makes the compiler writer's life easiest.
Module writers often use this flexibility to pur more important code earlier in a 
source file, relegating the plumbing work to a later time.

