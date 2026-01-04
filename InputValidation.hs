module InputValidation where 
import Types 
import FileIO

import Text.Read(readMaybe)
import Data.Char(isDigit,toLower, isSpace,toUpper)

--use the Either type to handle the Error and output the specific Error Message 
validateQuantity :: Int -> Either String Quantity 
validateQuantity q 
 | q < 0 = Left "Invalid input. Quantity cannot be negative!"
 | otherwise = Right (Quantity q)

--validate Price 
validatePrice :: Double -> Either String Price 
validatePrice p 
 | p <= 0 = Left "Error : Invalid input. Price cannot be negative!"
 | otherwise = Right (Price p)

--validate size 
validateSize :: Int -> Either String Size 
validateSize s 
 | s < 1 || s > 15 = Left "Error: Invalid size. Size must be between 1 and 15 (UK size)"
 | otherwise = Right (Size s)

--check if the shoeID format user enter valid or not
isValidFormat :: ShoeID -> Bool
isValidFormat ('S': rest) = length rest == 3 && all isDigit rest
isValidFormat _ = False

--validate shoesID 
validateShoeID :: [Shoe] -> ShoeID -> Either String ShoeID 
validateShoeID inventory sid 
 | null sid = Left "Shoe ID cannot be empty!"
 | not (isValidFormat sid) = Left "Invalid format! Shoes ID must start with 'S' follow by 3 digits number. Plese enter again. Ex (S001:) : "
 | any (\shoe -> shoeId shoe == sid) inventory = Left "This Shoe ID already exists! Please enter a unique ID."
 | otherwise = Right sid 

--getQuantity 
getQuantity :: String -> IO Quantity
getQuantity prompt =
    putStrLn prompt >>
    getLine >>= \quantity ->
        case readMaybe quantity of
            Nothing -> putStrLn "Invalid number! Please try again." >> getQuantity prompt 
            Just n  -> case validateQuantity n of
                          Right q -> return q
                          Left err -> putStrLn err >> getQuantity prompt 
--getPrice 
getPrice :: String -> IO Price 
getPrice prompt = 
    putStrLn prompt >>
    getLine >>= \price ->
        case readMaybe price of 
            Nothing -> putStrLn "Invalid number! Please enter a valid price." >> getPrice prompt
            Just n -> case validatePrice n of 
                       Right p -> return p 
                       Left err -> putStrLn err >> getPrice prompt 

--getSize 
getSize :: String -> IO Size 
getSize prompt = 
    putStrLn prompt >>
    getLine >>= \size ->
    case readMaybe size of 
        Nothing -> putStrLn "Invalid number! Please enter a valid size (UK 1- 15)." >> getSize prompt 
        Just n  -> case validateSize n of 
            Right s -> return s
            Left err  -> putStrLn err >> getSize prompt 

--getShoeID 
getShoeID :: [Shoe] -> IO (Maybe ShoeID)
getShoeID inventory = do
    putStrLn "Enter Shoe ID (Format S001) to add new shoe or 'Q' to cancel: "
    input <- fmap (map toUpper) getLine

    if input == "Q"
        then return Nothing
        else case validateShoeID inventory input of
            Right sid -> return (Just sid)
            Left err  -> putStrLn err >> getShoeID inventory


--normalise string 
normalize :: String -> String
normalize = map toLower . filter (not. isSpace)

--parse function 
parseBrand :: String -> Either String Brand 
parseBrand input = case normalize input of 
    "adidas" -> Right Adidas
    "nike" -> Right Nike 
    "vans" -> Right Vans 
    "newbalance" -> Right NewBalance
    "airjordan" -> Right AirJordan
    "converse" -> Right Converse 
    _ -> Left "Invalid Brand! Please enter a valid brand (Adidas, Nike, Vans, NewBalance, AirJordan, Converse)."

--getBrand function 
getBrand :: String -> IO Brand 
getBrand prompt = 
    putStrLn prompt >> 
    getLine >>= \brand -> 
        case parseBrand brand of 
            Right b -> return b 
            Left err -> putStrLn err >> getBrand prompt --if user enter the invalid brand, loop back to ask them enter again.

--getNonEmpty : a function to check if the input user enter is empty or not 
getNonEmpty :: String -> IO String 
getNonEmpty prompt = 
    putStrLn prompt >>
    getLine >>= \input ->
    if null (filter (not . isSpace) input) -- Checks if it's empty OR just spaces
        then putStrLn "Error: Input cannot be empty! Please try again." >> getNonEmpty prompt
        else return input

