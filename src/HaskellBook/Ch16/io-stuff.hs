
{-# LANGUAGE RankNTypes #-}

-- An example of RankNTypes in Haskell.
-- Why is this here? You need to go read Chapter 16 - IO Functor.
-- Let me repeat a key paragraph:
-- 
-- We talked about Functors as a means of lifting functions o ver structure so
-- that we may transform only the contents, leaving the structure alone. What
-- if we wanted to transform only the structure and leave the type argument to
-- that structure or type constructor alone? With this, we have arraived at
-- natural transformations. 
--
-- nat :: (f -> g) -> f a -> g a
--
-- This type is impossible because we cannot have higher-kinded types as
-- arguments types to the function type. What's the problem, though? It looks
-- like the type signature for `fmap`, doesn't it? Yet `f` and `g` in `f -> g`
-- are higher-kinded types. They must be, because they are the same f and g
-- that, later in the type signature, are taking arguments. But in those places
-- they are applied to their arguments, and so have kind `*`.
--
module Chapter16_IO where


type Nat f g = forall a . f a -> g a
