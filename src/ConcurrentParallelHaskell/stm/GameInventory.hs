{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent.STM
import Control.Monad

data Item = Scroll | Wand | Banjo deriving (Show, Ord, Eq)

newtype Gold = Gold Int deriving (Eq, Ord, Show, Num)
newtype HitPoint = HitPoint Int deriving (Eq, Ord, Show, Num)

type Inventory = TVar [Item]
type Health = TVar HitPoint
type Balance = TVar Gold

data Player =
  Player {
    balance :: Balance,
    health :: Health,
    inventory :: Inventory }


basicTransfer :: Num a => a -> TVar a -> TVar a -> STM ()
basicTransfer qty fromBal toBal = do
  from <- readTVar fromBal
  to <- readTVar toBal
  writeTVar fromBal (from - qty)
  writeTVar toBal (to - qty)

-- The properties of atomicity and isolation guarantee that if another thread
-- sees a change in bob's balance, they will also be able to see the
-- modification of alice's balance.
--
transferTest :: STM (Gold, Gold)
transferTest = do
  alice <- newTVar (12 :: Gold)
  bob <- newTVar 4
  basicTransfer 3 alice bob
  liftM2 (,) (readTVar alice) (readTVar bob)

removeInventory :: Eq a => a -> [a] -> Maybe [a]
removeInventory x xs = 
  case takeWhile (/= x) xs of
      (_ : ys) -> Just ys
      [] -> Nothing

maybeGiveItem :: Item -> Inventory -> Inventory -> STM Bool
maybeGiveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInventory item fromList of
      Nothing -> return False
      Just newList -> do
        writeTVar fromInv newList
        destItems <- readTVar toInv
        writeTVar toInv (item : destItems)
        return True
--
-- The main issue with [[maybeGiveItem]] is that the client of this particular
-- API has to check whether the item was given, and having to propagate an
-- indication of success back to the our caller. The complexity cascades
-- outwards... Another elegant way to resolve this particular problem is to use
-- [[retry]] which immediately terminate an atomically block that cannot
-- succeed.
--
giveItem :: Item -> Inventory -> Inventory -> STM ()
giveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInventory item fromList of
      Nothing -> retry
      Just newList -> do
        writeTVar fromInv newList
        readTVar toInv >>= writeTVar toInv . (item : )

-- essentially, this is just one block
testGivingItems :: STM (Bool, Bool, [Item], [Item])
testGivingItems = do
  inv0 <- newTVar [Scroll, Wand, Banjo]
  inv1 <- newTVar []
  a <- maybeGiveItem Wand inv0 inv1  -- <1>
  b <- maybeGiveItem Banjo inv0 inv1 -- <2> 
  items0 <- readTVar inv0
  items1 <- readTVar inv1
  return (a, b, items0, items1)

-- Demonstrating how one can improve [[basicTransfer]] using [[retry]]
--
transfer :: Gold -> Balance -> Balance -> STM ()
transfer qty fromBal toBal = do
  from <- readTVar fromBal
  when (qty > from) $ retry
  writeTVar fromBal (from - qty)
  readTVar toBal >>= writeTVar toBal . (qty +)


main :: IO ()
main = do
  r <- atomically transferTest
  putStrLn ("Here: " ++ show r)
  r2 <- atomically testGivingItems
  putStrLn ("Here: " ++ show r2)


-- obvious pattern to embed the idea of an alternative via `orElse`.
crummyList :: [(Item, Gold)] -> Player -> Player -> STM (Maybe (Item, Gold))
crummyList list buyer seller = go list
  where go [] = return Nothing
        go (this@(item, price) : rest) = do
          sellItem item price buyer seller
          return (Just this)
          `orElse` go rest

sellItem :: Item -> Gold -> Player -> Player -> STM ()
sellItem item price buyer seller = do
  giveItem item (inventory seller) (inventory buyer)
  transfer price (balance buyer) (balance seller)

maybeSTM :: STM a -> STM (Maybe a)
maybeSTM m = (Just `liftM` m) `orElse` return Nothing

shoppingList :: [(Item, Gold)] -> Player -> Player -> STM (Maybe (Item, Gold))
shoppingList list buyer seller = maybeSTM . msum $ map sellOne list
  where sellOne this@(item,price) = do
          sellItem item price buyer seller
          return this

-- since STM is an instance of the MonadPlus typeclass, we can generalize
-- maybeSTM to work over any MonadPlus
--
maybeM :: MonadPlus m => m a -> m (Maybe a)
maybeM m = (Just `liftM` m) `mplus` return Nothing


