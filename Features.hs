module Features where 

import Types 
import InputValidation 
import Display 
import FileIO 

import Data.List(find)
import Data.List (isInfixOf, sortOn)
import Data.Char (toUpper)


--helper function to find shoe based on Shoe ID 
findShoe :: [Shoe] -> ShoeID -> Maybe Shoe
findShoe inventory sid = find (\s -> shoeId s == sid) inventory

--helper function to replaced the old shoes by the new ones
replaceShoe :: Shoe -> [Shoe] -> [Shoe]
replaceShoe newShoe =
   map (\s -> if shoeId s == shoeId newShoe then newShoe else s)


--view low inventory funtion 
viewLowInventory :: [Shoe] -> IO ()
viewLowInventory inventory = do
   let lowInventory = sortByQuantity $ filter (\s -> quantity s <= Quantity 50) inventory
   if null lowInventory
       then putStrLn "No low inventory items."
       else displayInventory "LOW INVENTORY ITEMS" lowInventory


-- Sort inventory by ID
sortById :: [Shoe] -> [Shoe]
sortById = sortOn shoeId

-- Sort inventory by Quantity
sortByQuantity :: [Shoe] -> [Shoe]
sortByQuantity = sortOn quantity


-- helper function to update shoes 
performUpdate :: Shoe -> Quantity -> [Shoe] -> IO()
performUpdate shoe newQty inventory = do 
    let updatedShoe = shoe { quantity = newQty, status = checkStatus newQty}
    let updatedInventory = replaceShoe updatedShoe inventory 
    writeShoes updatedInventory 
    putStrLn $ "Stock updated successfully! Old stock: "
           <> showQuantity (quantity shoe)
           <> ", New stock: "
           <> showQuantity newQty


--add shoe function 
addShoe :: IO ()
addShoe = do
    inventory <- loadInventory
    printTitle "ADD NEW SHOE TO INVENTORY"

    maybeSid <- getShoeID inventory
    case maybeSid of
        Nothing -> putStrLn "Add shoe cancelled."
        Just sid -> do
            name     <- getNonEmpty "Enter Shoe Name: "
            model    <- getNonEmpty "Enter Shoe Model: "
            color    <- getNonEmpty "Enter Shoe Color: "
            brand    <- getBrand "Enter Brand: "
            size     <- getSize "Enter Size (UK 1-15): "
            quantity <- getQuantity "Enter Quantity: "
            price    <- getPrice "Enter Price: RM "

            let newShoe = Shoe
                  { shoeId    = sid
                  , name      = name
                  , model     = model
                  , color     = color
                  , brandName = brand
                  , size      = size
                  , quantity  = quantity
                  , price     = price
                  , status    = checkStatus quantity
                  }

            saveInventory newShoe
            putStrLn ""
            putStrLn $ "Shoe: " <> sid <> " " <> name <> " successfully added to inventory!"
            addShoe --loop back

--update shoe function 
updateShoeInfo :: IO ()
updateShoeInfo = do
    inventory <- loadInventory
    printTitle "UPDATE SHOE INFORMATION"

    putStrLn "Enter Shoe ID to update (Ex: S001) or 'Q' to return:"
    inputID <- fmap (map toUpper) getLine

    if inputID == "Q"
        then putStrLn "Returning to Main Menu..."
        else case findShoe inventory inputID of
            Nothing -> do
                putStrLn "Shoe not found! Please enter a valid shoe ID."
                updateShoeInfo
            Just shoe -> do
                printTitle "Current Shoe Information"
                printShoe shoe
                updateShoeMenu shoe inventory


updateShoeMenu :: Shoe -> [Shoe] -> IO ()
updateShoeMenu shoe inventory = do
    printTitle "UPDATE SHOE INFORMATION"
    putStrLn "Choose what to update:"
    putStrLn "1. Update Shoe Name"
    putStrLn "2. Update Shoe Model"
    putStrLn "3. Update Shoe Colour"
    putStrLn "4. Exit"

    choice <- getLine
    case choice of
        "1" -> do
            newName <- getNonEmpty "Enter new Shoe Name: "
            let updatedShoe = shoe { name = newName }
            let updatedInventory = replaceShoe updatedShoe inventory
            writeShoes updatedInventory
            putStrLn "Shoe name updated successfully!"
            updateShoeMenu updatedShoe updatedInventory

        "2" -> do
            newModel <- getNonEmpty "Enter new Shoe Model: "
            let updatedShoe = shoe { model = newModel }
            let updatedInventory = replaceShoe updatedShoe inventory
            writeShoes updatedInventory
            putStrLn "Shoe model updated successfully!"
            updateShoeMenu updatedShoe updatedInventory

        "3" -> do
            newColor <- getNonEmpty "Enter new Shoe Colour: "
            let updatedShoe = shoe { color = newColor }
            let updatedInventory = replaceShoe updatedShoe inventory
            writeShoes updatedInventory
            putStrLn "Shoe colour updated successfully!"
            updateShoeMenu updatedShoe updatedInventory

        "4" ->
            putStrLn "Returning to previous menu..."

        _ -> do
            putStrLn "Invalid option! Please try again."
            updateShoeMenu shoe inventory


updateStock :: IO()
updateStock = do 
    inventory <- loadInventory 
    printTitle "UPDATE SHOE STOCK" 
    putStrLn "Enter the Shoe ID to update stock (Ex: S001) or 'Q' to return to the main menu: "
    inputID <- fmap (map toUpper) getLine --normalising the id let user enter lowercase s001 also can match 
    --check for the user input, if they enter q or Q return to main menu 
    if inputID == "Q"  
        then putStrLn "Back to Main Menu"
        else case findShoe inventory inputID of 
            Nothing -> do 
                putStrLn "Shoe not found! Please enter a valid shoe id."
                updateStock --loop back to try again 
            
            Just shoe -> do 
                --putStrLn "\n-----------Current Shoe Information-------------"
                printTitle "Current Shoe Information"
                printShoe shoe --print the shoe information out for user 

                putStrLn $
                   "Choose an option:\n" <>
                    "1. Replace stock (Enter a new stock quantity)\n" <>
                    "2. Add new stock (add to current inventory)\n" <>
                    "3. Exit"


                option <- getLine 
                case option of 
                    "1" -> do 
                        putStrLn $ "Current Stock: " <> showQuantity (quantity shoe)
                        newQty <- getQuantity "Enter new stock quantity: "
                        performUpdate shoe newQty inventory 
                        updateStock --loop back to update stock 
                    
                    "2" -> do 
                        putStrLn $ "Current stock: " <> showQuantity (quantity shoe)
                        addedQty <- getQuantity "Enter quantity to add: "
                        let newQty = (quantity shoe) <> addedQty
                        performUpdate shoe newQty inventory 
                        updateStock --loop back to update stock 

                    
                    "3" -> putStrLn "Exit. Returning to Main Menu"
                    _ -> putStrLn "Invalid option! Please enter a valid choice." >> updateStock 

--update price
updatePrice :: IO ()
updatePrice = do
    inventory <- loadInventory
    printTitle "UPDATE SHOE PRICE"
    putStrLn "Enter the Shoe ID to update price (Ex: S001) or 'Q' to return to the main menu: "
    inputID <- fmap (map toUpper) getLine
    
    if inputID == "Q"
        then putStrLn "Back to Main Menu"
        else case findShoe inventory inputID of 
            Nothing -> do 
                putStrLn "Shoe not found! Please enter a valid shoe id."
                updatePrice -- loop back to try again 
            
            Just shoe -> do 
                printTitle "Current Shoe Information"
                printShoe shoe -- Reuse your printer!

                newP <- getPrice "Enter new price: RM "
                
                let updatedShoe = shoe { price = newP }
                let updatedInventory = replaceShoe updatedShoe inventory 
                
                writeShoes updatedInventory 
                putStrLn $ "Price updated successfully! Old price: RM" 
                       <> showPrice (price shoe) 
                       <> ", New price: RM" 
                       <> showPrice newP
                
                updatePrice  -- loop back to update another price

--delete shoe function 
deleteShoe :: IO ()
deleteShoe = do
    inventory <- loadInventory
    printTitle "DELETE SHOE"
    putStrLn "Enter the Shoe ID to delete from inventory (ex: S001) or 'Q' to cancel: "
    inputID <- fmap (map toUpper) getLine

    if inputID == "Q"
        then putStrLn "Delete cancelled. Returning to Main Menu."
        else case findShoe inventory inputID of
            Nothing -> do
                putStrLn "Shoe not found! Please enter a valid Shoe ID."
                deleteShoe  -- loop back to enter Shoe ID
            Just shoe -> do
                printTitle "Current Shoe Information"
                printShoe shoe
                confirmDelete shoe inventory  -- loop only the Y/N question
        
-- confirmation loop inside the same function
confirmDelete :: Shoe -> [Shoe] -> IO ()
confirmDelete shoe inventory = do
    putStrLn "Are you sure you want to delete this shoe from the inventory? (Y/N)"
    confirm <- fmap (map toUpper) getLine
    case confirm of
        "Y" -> do
            let updatedInventory = filter (\s -> shoeId s /= shoeId shoe) inventory
            writeShoes updatedInventory
            putStrLn $ "Shoe: " <> shoeId shoe <> " " <> name shoe <> " successfully deleted!"
            deleteShoe  -- loop back to delete another shoe
        "N" -> do
            putStrLn "Deletion cancelled."
            deleteShoe  -- loop back to delete another shoe
        _ -> do
            putStrLn "Invalid input! Please enter Y or N."
            confirmDelete shoe inventory  -- loop only the confirmation

-- View all inventory
viewInventory :: IO ()
viewInventory = do
    inventory <- loadInventory
    printTitle "VIEW INVENTORY MENU"
    if null inventory
        then putStrLn "Inventory is empty."
        else do
            --putStrLn "\n=== View Inventory Menu ==="
            putStrLn "1. View All Inventory"
            putStrLn "2. View Inventory Sorted by ID"
            putStrLn "3. View Inventory Sorted by Quantity"
            putStrLn "4. View Low Inventory Items"
            putStrLn "5. Exit"
            putStrLn ""
            putStr "Choice: "
            choice <- getLine
            case choice of
                "1" -> displayInventory "CURRENT INVENTORY" inventory >> pause >> viewInventory
                "2" -> displayInventory "INVENTORY SORTED BY ID " (sortById inventory) >> pause >> viewInventory
                "3" -> displayInventory "INVENTORY SORTED BY QUANTITY (LOW TO HIGH) " (sortByQuantity inventory) >> pause >> viewInventory
                "4" -> viewLowInventory inventory >> pause >> viewInventory
                "5" -> putStrLn "Exiting to Main Menu..."
                _   -> putStrLn "Invalid choice. Please enter 1, 2, 3, 4, or 5." >> viewInventory


--search inventory by shoe name 

-- Search for shoes by name 
searchByName :: IO ()
searchByName = do
    inventory <- loadInventory
    printTitle "SEARCH INVENTORY"
    
    -- Removed the extra putStr so you don't get a double prompt
    query <- getNonEmpty "Enter Shoe Name to search (or 'Q' to cancel): "
    
    if map toUpper query == "Q"
        then putStrLn "Returning to Main Menu..."
        else do
            let results = filter (\s -> normalize query `isInfixOf` normalize (name s)) inventory
            if null results
                then do 
                    putStrLn tableSeparator 
                    putStrLn $ "No shoes found matching: " <> query
                    putStrLn tableSeparator
                else 
                    displayInventory ("Search Results for: " <> query) results
    pause

