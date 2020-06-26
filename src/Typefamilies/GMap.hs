
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}

import Prelude hiding (lookup)

import Data.Char             (ord)
import qualified Data.Map as Map

-- There are 2 types of data families (1) top-level declarations and (2)
-- defined as part of a type-class definition.
--
u = undefined -- the laziness in me made me do it





--
-- Flavour 1 : top-level declaration
--
data family MyMap k :: * -> *

data instance MyMap () v = MyMapUnit (Maybe v)
data instance MyMap (a, b) v = MyMapPair (MyMap a (MyMap b v)) -- 

--
-- Flavour 2 : defined as part of a type-class
--

-- There is a bit of a mind-twister for me here because of the way type-classes
-- are typically used and imagine my face when i saw that i can declare a data
-- type in the type-class ;) 
--
class GMapKey k where -- the data type is defined in the type-class
  data GMap k :: * -> *
  empty :: GMap k v
  lookup :: k -> GMap k v -> Maybe v
  insert :: k -> v -> GMap k v -> GMap k v
  -- (2) devised the generalized form with constraints on the type class
  -- paramter "k".
  insertALL :: (GMapKey k, Foldable t) => t (k, v) -> GMap k v
  insertALL = foldr (\e -> \m -> insert (fst e) (snd e) m) empty --  (empty :: GMap k v)

-- this instance caters to generic maps using integers as keys
instance GMapKey Int where
  data GMap Int v        = GMapInt (Map.Map Int v) -- this name is exposed at the top-level
  empty                  = GMapInt Map.empty
  lookup k (GMapInt m)   = Map.lookup k m
  insert k v (GMapInt m) = GMapInt (Map.insert k v m)

-- (1) specialized form since common algorithmic patterns are obvious e.g.
-- folding / catamorphism.
insertAll :: Foldable t => t (Int, Int) -> GMap Int Int
insertAll xs =
  foldr (\e -> \m -> insert (fst e) (snd e) m) (empty :: GMap Int Int) xs

main :: IO ()
main = do
  let m  = empty :: GMap Int Int
      m2 = insert 1 2 m
      m3 = insertAll (zip [1..10] [1..10])
      m4 = insertALL (zip ['a'..'z'] (map (\e -> [e] ++ "1") ['a'..'z'])) :: GMap Char String
  putStrLn . show $ lookup 'f' m4
  putStrLn . show $ lookup 1 m2 -- something that exists
  putStrLn . show $ lookup 2 m2 -- something that does not exist
  putStrLn . show $ lookup 3 m3 -- something that exists
  return ()

-- this instance caters to generic maps using chars as keys
instance GMapKey Char where
  data GMap Char v        = GMapChar (GMap Int v)
  empty                   = GMapChar empty
  lookup k (GMapChar m)   = lookup (ord k) m
  insert k v (GMapChar m) = GMapChar (insert (ord k) v m)


-- this instance caters to generic maps using () or unit as keys
-- i cannot imagine why such an instance exists but if it compiles ... then
-- maybe its ok ?
instance GMapKey () where
  data GMap () v           = GMapUnit (Maybe v)
  empty                    = GMapUnit Nothing
  lookup () (GMapUnit m)   = m
  insert () v (GMapUnit _) = GMapUnit $ Just v

-- another instance that caters to 2-tuples! and we would like to construct a
-- k0 -> (k1 -> v) i.e. a multi-map and the associated type synonym approach
-- allows me to write such an instance.
--
-- the code that is interesting is found in "insert" where we search whether k0
-- is mapped to k1 and there are 2 outcomes:
-- (a) if k0 does not map to k1, then we create an association
-- (b) if k0 does map to k1, then we associate k1 to replace the value "v" in
-- the inner map.
instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a,b) v = GMapPair (GMap a (GMap b v))
  empty = GMapPair empty
  lookup (a, b)   (GMapPair mm) = lookup a mm >>= lookup b
  insert (a, b) v (GMapPair mm) = GMapPair $ case lookup a mm of
    Nothing -> insert a (insert b v empty) mm
    Just mm2 -> insert a (insert b v mm2) mm 


-- from the previous example where i can write a "map -> map" implementation,
-- it does look like i am able to also represent other more exotic key types
--
instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
  empty = GMapEither empty empty
  lookup (Left  e)   (GMapEither gma _) = lookup e gma
  lookup (Right a)   (GMapEither _ gmb) = lookup a gmb
  insert (Left  e) v (GMapEither gma gmb) = GMapEither (insert e v gma) gmb
  insert (Right a) v (GMapEither gma gmb) = GMapEither gma (insert a v gmb)


