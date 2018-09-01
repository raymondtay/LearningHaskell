-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
--

  {-
    if i were to load this file which includes the duplicate function declaration, then 
    i'm going to see this error message:

    Plus.hs:11:10: error:
    Duplicate instance declarations:
      instance [safe] Plus Int Float Int -- Defined at Plus.hs:11:10
      instance Plus Int Float Int -- Defined at PlusPlus.hs:6:10
   |
11 | instance Plus Int Float Int where plus x y = (+) x (round y)
   |          ^^^^^^^^^^^^^^^^^^
  -}
import Plus -- the definitions are loaded first

instance Plus Int Float Int where plus x y = (+) x (round y) -- the definitions are loaded second which results in a conflict

