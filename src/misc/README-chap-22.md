# Readers

When writing applications, programmers often need to pass around
some information that may be needed intermittently or
universally throughout an entire application. We don't want to simply
pass this information as arguments because it would be present in the 
type of almost every function. This can make the code harder to read and
harder to maintain. To address this, we use the `Reader Monad`.

