module Display where 

import Types

-- Helper functions to unwrap newtypes for display
showSize :: Size -> String 
showSize (Size s) = show s

showQuantity :: Quantity -> String
showQuantity (Quantity q) = show q

showPrice :: Price -> String
showPrice (Price p) = show p

--table formatting 
-- Column widths for: ID, Name, Model, Brand, Size, Qty, Price, Status
colWidths :: [Int]
colWidths = [6, 18, 12, 12,12, 6, 6, 10, 12]

-- Total table width helper (sum of columns + spaces + borders)
totalTableWidth :: Int
totalTableWidth = sum colWidths + (length colWidths - 1) * 3 + 4  -- 3 for " | " between columns, 2 for "| "

-- Reusable table separator line
tableSeparator :: String
tableSeparator = replicate totalTableWidth '-' 


formatRow :: [String] -> [Int] -> String
formatRow rowValues widths =
   let paddedValues = map (\(v,w) -> v <> replicate (max 0 (w - length v)) ' ') (zip rowValues widths)
       rowString = map (\v -> v <> " | ") paddedValues
   in "| " <> mconcat rowString 

-- Table header using reusable separator
tableHeader :: String
tableHeader =
   let headers = ["ID", "Name", "Model", "Brand", "Color", "Size", "Qty", "Price", "Status"]
   in tableSeparator <> "\n" <> formatRow headers colWidths <> "\n" <> tableSeparator
 

-- Convert a Shoe to a formatted row
shoeToRow :: Shoe -> String
shoeToRow s =
   formatRow
       [ shoeId s
       , name s
       , model s
       , show (brandName s)
       , color s
       , showSize (size s)
       , showQuantity (quantity s)
       , "RM" <> showPrice (price s)
       , show (status s)
       ] colWidths


printShoe :: Shoe -> IO ()
printShoe = putStr . displayShoe 


displayShoe :: Shoe -> String 
displayShoe shoe = 
  "Shoe ID : " <> shoeId shoe <> "\n"
 <> "Name : " <> name shoe <> "\n"
 <> "Model : " <> model shoe <> "\n"
 <> "Brand : " <> show (brandName shoe) <> "\n"  
 <> "Color : " <> color shoe <> "\n"
 <> "Size : " <> showSize (size shoe) <> "\n"
 <> "Quantity : " <> showQuantity (quantity shoe) <> "\n"
 <> "Price : RM" <> showPrice (price shoe) <> "\n"
 <>  "Status : " <> show (status shoe) <> "\n"
 <> tableSeparator <> "\n"


displayInventory :: String -> [Shoe] -> IO ()
displayInventory title inventory = do
   printTitle title
   putStrLn tableHeader
   mapM_ (putStrLn . shoeToRow) inventory
   putStrLn tableSeparator



printTitle :: String -> IO ()
printTitle title = do
   putStrLn tableSeparator
   putStrLn (centerText totalTableWidth title)
   putStrLn tableSeparator


centerText :: Int -> String -> String
centerText width text =
   let textLen = length text
       leftPad = max 0 ((width - textLen) `div` 2)
   in replicate leftPad ' ' <> text


--helper to help print the view inventory function more smooth 
pause :: IO ()
pause = do
    putStrLn "\nPress Enter to continue..."
    _ <- getLine  -- This waits for the user to type something and hit Enter
    return ()     -- Then it finishes and lets the program move on
 