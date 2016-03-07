
{-
 - What if we want to do something different ?
 -
 - We talked about functor as a means of lifting functions over structure
 - so that we may transofrm only the contents, leaving the structure alone.
 - What if we wanted to transform only the structure and leave the type
 - argument to that structure or type constructor alone?
 - With this, we have arrived at natural transformations. We can attempt to put
 - together a type to express what we want:
 -
 - nat :: (f -> g) -> f a -> ga
 -
 - This type is impossible because we cannot have higher-kinded types as argument
 - types to the function type. What's the problem, though?
 - It looks like the type signature for fmap doesn't it?
 - Yet f and g in f -> g are higher-kinded types. They must be, because they are
 - the same f and g that, later in the type signature are taking arguments. But 
 - in those places they are applied to their arguments and so have kind *.
 - 
 - However, not all hope is lost ! We can make a modest change to fix it
 -}

{-# LANGUAGE RankNTypes #-}

module MoreAboutFunctors where

type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- here's another way to write the same expression
type Nat' f g a = f a -> g a
degenerateMtl :: Num a => Nat' Maybe [] a
degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a+1]
-- this style of writing functionality is discouraged as it's better
-- to write that functionality using a "fold".

degenerateMtl (Just a) = map (+1) [a]

--  The forall keyword is used to explicitly bring type variables into scope.
--  For example, consider something you have innocuously seen written a 
--  hundred times so far:
--
--  map :: (a -> b ) -> [a] -> [b]
--  
--  But what are a and b? Well, they are type variables, you asnwer. The compilers
--  sees that they begin with a lowercase letter and as such allows any type
--  to fill that role. Another way of putting this is that those variables are 
--  universally quantified. If you have studied formal logic, you will have 
--  undoubtedly come across the quantifiers. 'for all' and exists quantify whatever
--  comes after them.
--  Hence, the other way to write them is 
--
--  map :: forall a b . (a -> b) -> [a] -> [b]
--
--  Another thing is that the following two equations are equivalent:
--  
--  id :: a -> a
--  id :: forall a . a -> a 
--
-- data ShowBox = forall s . Show a => SB s
-- heteroList :: [ShowBox]
-- heteroList = [SB (), SB 5, SB True]
--
--
