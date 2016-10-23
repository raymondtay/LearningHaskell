# Monad Transformers

## Lexically inner is structurally outer

Lifted from Section 26.8 of the book ...

One of the trickier parts of monad transformers is that lexical representation
of the types will violate your intuitions with respect to the relationship
it has with the structure of your values. Let us note something in the definitions
of the following types:
```haskell

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

```

A necessary byproduct of how transformers work is that the additional structure
`m` is always wrapped _around_ our value. One thing to note is that it's only 
wrapped around things we can have, not things we _need_, such as `ReaderT`. 
The consequence of this is that a series of monad transformers in a type will begin
with the innermost type structurally speaking.

