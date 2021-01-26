module Main where
import UserMenu
import Control.Monad
import System.Directory


main :: IO ()
main = do
    putStrLn "*******************************************************************************************************************"
    putStrLn "*                               --------Harry Potter Web Crawler---------                                         *"
    putStrLn "*     Press Any key and then press Enter to download the main actors and characters info into our database        *"
    putStrLn "*                                Press Enter without any value to quit                                            *"
    putStrLn "*******************************************************************************************************************"
    option <- getLine
    when (not $ null option) $ do
      print "Please Wait...."
      bool <- doesFileExist "HarryPotterDB.db"  --checks whether the .db file exists or not, returns true if it exists.
      if bool
      then do removeFile "HarryPotterDB.db"   -- if the database exists, this function call removes the file to avoid duplication of data before downloadinfo function is called.
              downloadCastInfo
      else do downloadCastInfo
      userMenu      -- userMenu function is called after the database is populated to start the user interface
