module Types where 

--data types

--type synonyms 
type ShoeID = String
type Color = String 


newtype Price = Price Double deriving (Show, Eq, Read,Ord)
newtype Quantity = Quantity Int deriving (Show, Eq, Read,Ord)
newtype Size = Size Int deriving (Show, Eq, Read,Ord)

--implement typeclass
instance Semigroup Quantity where 
  (<>) :: Quantity -> Quantity -> Quantity
  (Quantity a) <> (Quantity b) = Quantity (a + b)


--custom type 
data Shoe = Shoe {
    shoeId :: ShoeID,
    name :: String,
    model :: String,
    brandName :: Brand, 
    color :: String,
    size :: Size, 
    quantity :: Quantity,
    price :: Price,
    status :: StockStatus 
} deriving (Show, Read)

data Brand = Adidas | Nike | Vans | NewBalance | AirJordan | Converse deriving (Show,Eq,Read)

--shoes status 
--change to stock status 
data StockStatus = LowStock | InStock | OutOfStock deriving (Show, Eq, Read) 

--check status 
checkStatus :: Quantity -> StockStatus 
checkStatus (Quantity q)
  | q == 0 = OutOfStock
  | q <= 50 = LowStock
  | otherwise = InStock 

