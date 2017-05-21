# This is recorded on 21 May 2017
----------


```haskell
  newtype Reader r a = Reader { runReader :: r -> a }

  newtype Writer w a = Writer { runWriter :: (a, w) }

  newtype State s a = State { runState :: s -> (a, s) }

  newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

  newtype WriterT w m a = WriterT { runWriterT :: m (a , w)  }

  newtype StateT s m a = StateT { runStateT :: s -> m (a ,s) }

```


For the purposes of the progression we are trying to demonstrate here, it
suffcices to know that the Writer Applicative and Monad work by combining the w
values monoidally. With that in mind, what we can see is that Reader lets us
talk about values we need, Writer lets us deal with values we can emit and
combine but not read, and State lets us read and write values in any manner we
desire - including monoidally, like Writer.

this is one reason you need not boether with Writer sice State can replace it
anyway. Now you know why you don't need Writer; we'll talke more about why you
don't want Writer later.

In fact, there is a type in the `transformers` library that combines Reader,
Writer and State into one big type.

```haskell
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }
```
## Why not use Writer or WriterT?

It is a bit too easy to get into a situation where Writer is either too lazy or
too strict for the problem you are solving, and then it will use more memory
than you would like. Writer can accumulate unevaluated thunks, causing memory
leaks. It is also inappropriate for logging long running or ongoing programs
due to the fact that you cannot retrieve any of the logged values untill the
computation is complete.

## Lexically inner is structurally outer

One of the trickier parts of monad transformers is that the lexical
representation of the types will violate your intuitions w.r.t the relationship
it has with the structure of your values. Let us note something in the
definitions of the following types:

```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a)) }

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

A necessary byproduct of how transformers work is that the additional strucutre
`m` is always wrappedm around our value. One thing to take is that it is only
wrapped around things we can have, not things we need, such as with `ReaderT`.

The consequence of this is that a series of monad transformers in a type will
begi with the innermost type structurally speaking.
