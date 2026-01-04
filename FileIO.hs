module FileIO where 

import Types
import Text.Read(readMaybe)
import Data.Maybe(mapMaybe)

-- The Constant (File Path)
shoesInventoryFile :: FilePath
shoesInventoryFile = "shoes.txt"

--seprate each 4 function to do all the file stuff
shoeToLine :: Shoe -> String
shoeToLine = show 

--save the file to the inventory 
saveInventory :: Shoe -> IO ()
saveInventory shoe = do
    appendFile shoesInventoryFile (shoeToLine shoe ++ "\n")

--load evryting inside the inventory
loadInventory :: IO [Shoe]
loadInventory = do
    content <- readFile shoesInventoryFile
    let shoes = mapMaybe readMaybe (lines content)
    return $ length shoes `seq` shoes

--write the shoes 
writeShoes :: [Shoe] -> IO ()
writeShoes shoes = do
    let content = unlines (map shoeToLine shoes)
    writeFile shoesInventoryFile content


