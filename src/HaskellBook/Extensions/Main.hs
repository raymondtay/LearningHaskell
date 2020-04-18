{-#LANGUAGE BangPatterns #-}

data ShoppingItem = ShoppingItem { name :: String, cost :: Double } deriving Show

class Priceable p where
  getPrice :: p -> Double

instance Priceable ShoppingItem where
  getPrice (ShoppingItem _ cost) = cost

totalPrice :: Priceable p => [p] -> Double
totalPrice ps = sum $ map getPrice ps

main :: IO ()
main = do
  let items = [ShoppingItem { name = "lettuce", cost = 2.3 } , ShoppingItem {name = "Turkey Ham" , cost = 6.5}]
  putStrLn $ "Total price: $ " ++ show (totalPrice items)

