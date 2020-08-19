{-# LANGUAGE TypeFamilies #-}

import qualified Data.Vector.Unboxed as V

-- Data Families otoh, allows us to create new type parameterized data
-- constructors. Normally, we can only define typeclasses functions whose
-- behavior results in a uniform result which is purely a result of the
-- typeclasses arguments. With data families we can allow specialized behavior
-- indexed on the type.
--

data family Array a
data instance Array Int = IArray (V.Vector Int)
data instance Array Bool = BArray (V.Vector Bool)
data instance Array (a, b) = PArray (Array a) (Array b)
data instance Array (Maybe a) = MArray (V.Vector Bool) (Array a)

class IArray a where
  index :: Array a -> Int -> a

instance IArray Int where
   index (IArray xs) i = xs V.! i

instance IArray Bool where
  index (BArray xs) i = xs V.! i

instance (IArray a, IArray b) => IArray (a, b) where
  index (PArray xs ys) i = (index xs i, index ys i)

instance (IArray a) => IArray (Maybe a) where
  index (MArray bs xs) i =
    case bs V.! i of
      True  -> Nothing
      False -> Just $ index xs i


