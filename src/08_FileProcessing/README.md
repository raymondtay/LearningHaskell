

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

