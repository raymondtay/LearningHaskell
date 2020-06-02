{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.Par
import Data.Set (Set)
import qualified Data.Set as S

data Client = GovOrg { clientName :: String }
            | Company { clientName :: String, person :: Person, duty :: String }
            | Individual { person :: Person } deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Eq, Ord, Generic, NFData)

data Person = Person { firstName :: String, lastName :: String, gender :: Gender } deriving (Show, Eq, Ord)

data Gender = Male | Female | UnknownGender deriving (Show, Eq, Ord, Generic, NFData)

data Product = Product  { productId :: Integer, productType :: ProductType } deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TravelGuide | Tool | Trip deriving (Show, Eq, Ord, Generic, NFData)

data Purchase = Purchase { client :: Client, products :: [Product] } deriving (Show, Eq, Ord)

data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty String
                  | InfoClientGender Gender
                  | InfoPurchasedProduct Integer
                  | InfoPurchasedProductType ProductType
                  deriving (Show, Eq, Ord, Generic, NFData)

newtype FrequentSet = FrequentSet (Set PurchaseInfo) deriving (Eq, Ord, Generic, NFData)
newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
  let total = length trans
      f (Transaction tElts) = S.isSubsetOf sElts tElts
      supp = length (filter f trans)
   in fromIntegral supp / fromIntegral total

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList


printTicket :: Int -> Int -> [(Int, String)] -> [(Int, String)] -> String
printTicket idC idP clients products = runPar $ do
  clientV <- new
  productV <- new
  fork $ lookupPar clientV idC clients
  fork $ lookupPar clientV idP products
  envV <- new
  letterV <- new
  fork $ printEnvelope clientV envV
  fork $ printLetter clientV productV letterV
  envS <- get envV 
  letterS <- get letterV
  return $ envS ++ "\n\n" ++ letterS


lookupPar :: (Eq a , NFData b) => IVar (Maybe b) -> a -> [(a, b)] -> Par ()
lookupPar i _ [] = put i Nothing
lookupPar i x ((k, v) : r) | x == k = put i $ Just v
  | otherwise = lookupPar i x r


printEnvelope :: IVar (Maybe String) -> IVar String -> Par ()
printEnvelope clientV envV = do
  clientName <- get clientV
  case clientName of
    Nothing -> put envV "unknown"
    Just n -> put envV $ "To: " ++ n

printLetter :: IVar (Maybe String) -> IVar (Maybe String) -> IVar String -> Par ()
printLetter clientV productV letterV = do
  clientName <- get clientV
  productName <- get productV
  case (clientName, productName) of
    (Nothing, Nothing) -> put letterV "Unknown"
    (Just n, Nothing) -> put letterV $ n ++ " bought something."
    (Nothing, Just n) -> put letterV "Unknown"
    (Just n, Just p) -> put letterV "Unknown"

-- sequential
generateL1S minSupp transactions =
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
      l1NotFiltered = map (\fs -> (fs, setSupport transactions fs > minSupp)) c1
  in concatMap (\(fs, b) -> if b then [fs] else []) l1NotFiltered

-- parallel 
generateL1P minSupp transactions = runPar $ do
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
  l1NotFiltered <- parMap (\fs -> (fs, setSupport transactions fs > minSupp)) c1
  return $ concatMap (\(fs, b) -> if b then [fs] else []) l1NotFiltered


generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupp transactions (k, lk) =
  let ck1 = noDups $ [ FrequentSet $ S.union a b | FrequentSet a <- lk, FrequentSet b <- lk, S.size (S.intersection a b) == k - 1 ]
      lk1 = runPar $ filterLk minSupp transactions ck1
   in Just (lk1, (k+1, lk1))

filterLk :: Double -> [Transaction] -> [FrequentSet] -> Par [FrequentSet]
filterLk minSupp transactions ck =
  let lengthCk = length ck
   in if lengthCk <= 5
         then return $ filter (\fs -> setSupport transactions fs > minSupp) ck
         else let (l, r) = splitAt (div lengthCk 2) ck
               in do lVar <- spawn $ filterLk minSupp transactions l
                     lFiltered <- get lVar
                     rVar <- spawn $ filterLk minSupp transactions r
                     rFiltered <- get rVar
                     return $ lFiltered ++ rFiltered


