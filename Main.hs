module Main where 

import Features 
import Display 



main :: IO ()
main = do
    putStrLn tableSeparator
    putStrLn "===========================================Shoe Inventory Management System ==============================================" 
    menuLoop


menuLoop :: IO ()
menuLoop = do
    printTitle "Main Menu"
    putStrLn "1. Add New Shoe"
    putStrLn "2. Update Shoe Information (name, model, size)" 
    putStrLn "3. Update Shoe Stock"
    putStrLn "4. Update Shoe Price"
    putStrLn "5. Delete Shoe"
    putStrLn "6. View Shoe Inventory"
    putStrLn "7. Search inventory by shoe name"
    putStrLn "8. Exit"
    
    putStrLn tableSeparator
    putStr "Enter your choice (1-8): "

    choice <- getLine
    case choice of
        "1" -> addShoe >> menuLoop
        "2" -> updateShoeInfo >> menuLoop
        "3" -> updateStock >> menuLoop
        "4" -> updatePrice >> menuLoop
        "5" -> deleteShoe >> menuLoop
        "6" -> viewInventory >> menuLoop
        "7" -> searchByName >> menuLoop
        "8" -> putStrLn "Goodbye! Thank you for using this system!"
        _   -> putStrLn "Invalid choice! Please enter number between 1 to 8." >> menuLoop

