{-# LANGUAGE ExistentialQuantification #-}

-- The idea of existential quantification in data type declarations was
-- suggested by Perry and implemented in Hope+. Let us go over what is meant by
-- that.
--

data Foo = forall a . MkFoo a (a -> Bool) | Nil

-- data type `Foo` has 2 constructors with types:
-- MkFoo :: forall a . a -> (a -> Bool) -> Foo
-- Nil :: Foo
--

xs :: [Foo]
xs = [MkFoo 3 even, MkFoo 4 odd]

-- what this allows us to do is package heterogeneous values together with a
-- bunch of functions that manipulate them, and then treat that collection of
-- packages in a uniform manner. You can express quite a bit of OO like
-- programming this way.
--
f :: Foo -> Bool
f (MkFoo val fn) = fn val

results = map f xs :: [Bool] -- this should give us a list of booleans values.
-- What about existentials and typeclasses? 
--
data Baz = forall a . Eq a => Baz1 a a
         | forall b . Show b => Baz2 b (b -> b)

-- like before, the types of these functions are 
-- Baz1 :: forall a . Eq a => a -> a -> Baz
-- Baz2 :: forall a . Show a => a -> (a -> a) -> Baz
--

-- Participating in pattern matching like any other functionality.
g :: Baz -> String
g (Baz1 p q) | p == q = "Yes"
             | otherwise = "No"
g (Baz2 v f) = show (f v)

--
-- The existential type found in Counter hides all functions that references
-- it apparently. You will discover this when you load records like these into
-- GHCi and you can contrast to the regular Haskell record declarations.
-- In otherwords, i lifted the following from file:///Library/Haskell/ghc-8.4.3-x86_64/doc/ghc-doc/users_guide/glasgow_exts.html?highlight=derivedatatypeable#extension-ExistentialQuantification
-- """
-- GHC defines a record selector function for fields whose type does not
-- mention the existentially-quantified variables.(This example used an
-- underscore in the fields for which record selectors will not be defined but
-- that is only programming style; GHC ignores them.)
-- """
data Counter' a = NewCounter' {
  _this'    :: a,
  _inc'     :: a -> a,
  _display' :: a -> IO (),
  tag'      :: String }

data Counter a = forall b . NewCounter {
 _this    :: b,
 _inc     :: b -> b,
 _display :: b -> IO (),
 tag      :: a }

-- An example, lifted from that page:
--
inc :: Counter a -> Counter a
inc (NewCounter x i d t) = NewCounter { _this = i x, _inc = i, _display = d, tag = t }

display :: Counter a -> IO ()
display NewCounter{ _this = x, _display = d } = d x

-- let's provide a few more instances

counterA :: Counter String
counterA = NewCounter { _this = 0, _inc = (1+), _display = print, tag = "A" }

counterB :: Counter String
counterB = NewCounter { _this = "", _inc = ('#':), _display = putStrLn, tag = "B" }

counterC :: Counter' Int
counterC = NewCounter' { _this' = 0, _inc' = (1+), _display' = print, tag' = "C" }

inc' :: Counter' a -> Counter' a
inc' (NewCounter' x i d t) = NewCounter' { _this' = i x, _inc' = i, _display' = d, tag' = t }

display' :: forall a . Counter' a -> IO ()
display' NewCounter'{ _this' = x, _display' = d } = d x

-- Its pretty nifty to use this approach; 
main = do
  display (inc counterA)
  display (inc (inc (inc counterB)))
  display' (inc' counterC)
  display' (inc' (inc' (inc' counterC)))


