-- BookInfo's the "type constructor", Book is the "value constructor"
-- Constructors consisted of built-in types in Haskell
data BookInfo = Book Int String [String] 
                deriving (Show)

data MagazineInfo = Magazine Int String [String] 
                    deriving (Show)

-- Usage of type synonyms, purpose is to create meaningful names
-- for the purpose of improving readability and reasoning 
data BookReview = BookReview BookInfo CustomerID ReviewBody
type ReviewBody = String
type CustomerID = Int
type BookRecord = (BookInfo, BookReview) 

-- Another example of how we might represent billing information
-- what is interesting is that we can use CustomerID again and this reflects
-- the fact that once a type is established in Haskell, it's global
type CardHolder = String
type CardNumber = String
type Address    = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address 
                   | CashOnDelivery
                   | Invoice CustomerID
                    deriving (Show)

-- Create higher abstractions with 'Double' type in Haskell
type Vector = (Double, Double) 
data Shape = Circle Vector Double
             | Poly [Vector]
             deriving (Show)


-- Pattern Matching in Haskell with wildcard for i-dont-cares
bookID (Book id _ _) = id
bookTitle (Book _ title _) = title
bookAuthors (Book _ _ authors) = authors
-- Haskell's record syntax for record definition and f.o.c getters/setters
data Customer = Customer {
        customerID :: CustomerID,
        customerName :: String,
        customerAddress :: Address } deriving (Show)

