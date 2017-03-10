{-# LANGUAGE InstanceSigs #-}

module SomeCodeFromBook where

data Query = Query 
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- there's a decoder function that makes some object from String
--
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined 

-- There's a query, that runs against DB and returns array of strings
--
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- there's some additoinal context initializer, that also has IO side-effects
--
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
      (Left err) -> return $ Left $ err
      (Right res) -> do
        a <- makeIoOnlyObj res
        return $ Right a

--
-- Chapter 21's exercises on writing a Traversable for Identity datatype
--
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
  -- traverse f = sequenceA . fmap f
  -- Above ↑ is the definition found in GHC ; when i dissect them i discover that
  -- sequenceA is actually `traverse id` which is evaluate each structure from
  -- left-to-right and then collect the results.
  --
  traverse f (Identity a) = traverse id $ fmap f (Identity a)


newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
  fmap f (Constant b) = Constant b

instance Monoid (Constant a b) where
  mempty = undefined
  mappend = undefined
  
instance Foldable (Constant a) where
  foldMap :: Monoid m => (b -> m) -> (Constant a) b -> m
  foldMap f (Constant g) = undefined

instance Traversable (Constant a) where
  traverse :: Applicative f => (b -> f c) -> (Constant a) b -> f (Constant a c)
  traverse f (Constant g) = traverse id $ fmap f (Constant g)

data Optional a = Nada | Yep a 

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada r = r 
  mappend l Nada = l
  mappend (Yep a) (Yep b) = Yep $ mappend a b

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap :: Monoid m => (a -> m) -> Optional a -> m
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse f Nada = traverse id $ fmap f Nada
  traverse f (Yep a) = traverse id $ fmap f (Yep a)

