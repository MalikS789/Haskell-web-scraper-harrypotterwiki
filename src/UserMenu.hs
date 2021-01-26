-- |This module basically creates the user interface that calls and utilizes all the functions imported from CharacterScraper, CastScraper and ToDB modules
-- It also has some useful functions that beautifies the user interface on the console
module UserMenu where

import qualified CharacterScraper as Ch
import qualified CastScraper as C
import Database.HDBC
import Database.HDBC.Sqlite3
import ToDB



mainActors = ["Daniel_Radcliffe", "Rupert_Grint", "Emma_Watson", "Robbie_Coltrane", "Alan_Rickman", "Maggie_Smith", "Tom_Felton","Ralph_Fiennes","Michael_Gambon","David_Thewlis", "Gary_Oldman", "Helena_Bonham_Carter", "Brendan_Gleeson", "Natalia_Tena","Matthew_Lewis","Poppy_Miller","Evanna_Lynch", "Katie_Leung"]



-- |This function first calls the getActorInfo and newCharacter functions from CastScraper and CharacterScraper modules respectively, to download information from the website
-- It then calls the batchInsertActor and batchInsertCharacter functions from ToDB module to create and populate the sqlite database.
downloadCastInfo:: IO ()
downloadCastInfo = do
  actors <- mapM C.getActorInfo mainActors
  characters <- mapM Ch.newCharacter $ map C.role actors
  batchInsertActor actors
  batchInsertCharacter characters
  putStrLn $ "Successfully downloaded data into database"
  putStrLn $ ""

-- |This function performs a DELETE sql statement to clear all the data from the two table
deleteRecordFromDatabase:: IO ()
deleteRecordFromDatabase = do
  conn <- connectSqlite3 "HarryPotterDB.db"
  run conn "DELETE FROM cast" []
  run conn "DELETE FROM character" []
  commit conn

-- |This functions prints an Actor information queried from the database in a beautiful and structured way.
printActor::ActorFromDB -> IO ()
printActor actor = do
  putStrLn "------------------------------------------------"
  putStrLn $ "Actor Full Name: " ++ " " ++(name $ actor)
  putStrLn "------------------------------------------------"
  putStrLn $ "Actor Date of birth: " ++ " " ++(dob $ actor)
  putStrLn "------------------------------------------------"
  putStrLn $ "Actor Nationality: " ++" " ++ (nationality $ actor)
  putStrLn "------------------------------------------------"
  putStrLn $ "Actor Gender: " ++ " " ++(cast_gender $ actor)
  putStrLn "------------------------------------------------"
  putStrLn $ "Actor Role: " ++ " " ++(role $ actor)
  putStrLn "------------------------------------------------"
  putStrLn ""

-- |This functions prints a Character information queried from the database in a beautiful and structured way.
printCharacter::CharacterFromDB -> IO ()
printCharacter character = do
  putStrLn "------------------------------------------------"
  putStrLn $ "Character Full Name: " ++ (fname $ character)
  putStrLn "------------------------------------------------"
  putStrLn $ "Character Gender: " ++ (gender $ character)
  putStrLn "------------------------------------------------"
  putStrLn $ "Character Blood Status: " ++ (bloodStatus $ character)
  putStrLn "------------------------------------------------"
  putStrLn $ "Character House: " ++ (house $ character)
  putStrLn "------------------------------------------------"
  putStrLn $ "Character Patronus: " ++ (patronus $ character)
  putStrLn "------------------------------------------------"
  putStrLn ""

-- |This functions queries the cast table to fetch info about an actor with a particular chosen by the user
searchByActorName::IO ()
searchByActorName = do
  putStrLn "*****************************************************************************************"
  putStrLn "*             Enter a number to learn about the actor from Harry Potter                 *"
  putStrLn "*                                                                                       *"
  putStrLn "*   1 > Daniel Radcliffe         2 > Rupert Grint              3 > Emma Watson          *"
  putStrLn "*   4 > Robbie Coltrane          5 > Alan Rickman              6 > Maggie Smith         *"
  putStrLn "*   7 > Tom Felton               8 > Ralph Fiennes             9 > Michael Gambon       *"
  putStrLn "*  10 > David Thewlis           11 > Gary Oldman              12 > Helena Bonham Carter *"
  putStrLn "*  13 > Brendan Gleeson         14 > Natalia Tena             15 > Matthew Lewis        *"
  putStrLn "*  16 > Poppy Miller            17 > Evanna Lynch             18 > Katie Leung          *"
  putStrLn "*****************************************************************************************"
  option <- getLine
  let parseInt = read option :: Int
  actorDoesntExist <- actorNotInDB parseInt
  if actorDoesntExist
  then do putStrLn "Invalid number entered, check and try again"
          searchByActorName
  else do
    actor <- fetchAllValuesActor parseInt
    printActor $ head actor
    putStrLn "The Actor potrays the following character in Harry Potter:"
    putStrLn ""
    characterFromDB <- fetchAllValuesCharacter parseInt
    printCharacter $ head characterFromDB


-- |This functions queries the character table to fetch info about a character with a particular chosen by the user
searchByCharacterName::IO ()
searchByCharacterName = do
  putStrLn "*****************************************************************************************"
  putStrLn "*             Enter a number to learn about the Character from Harry Potter             *"
  putStrLn "*                                                                                       *"
  putStrLn "*   1 > Harry Potter             2 > Ronald Weasley            3 > Hermione Granger     *"
  putStrLn "*   4 > Rubeus Hagrid            5 > Severus Snape             6 > Minerva McGonagall   *"
  putStrLn "*   7 > Draco Malfoy             8 > Tom Riddle/Voldemort      9 > Albus Dumbledore     *"
  putStrLn "*  10 > Remus Lupin             11 > Sirius Black             12 > Bellatrix Lestrange  *"
  putStrLn "*  13 > Alastor Moody           14 > Nymphadora Tonks         15 > Neville Longbottom   *"
  putStrLn "*  16 > Ginevra Weasley         17 > Luna Lovegood            18 > Cho Chang            *"
  putStrLn "*****************************************************************************************"
  option <- getLine
  let parseInt = read option :: Int
  actorDoesntExist <- actorNotInDB parseInt
  if actorDoesntExist
  then do putStrLn "Invalid number entered, check and try again"
          searchByActorName
  else do

    characterFromDB <- fetchAllValuesCharacter parseInt
    printCharacter $ head characterFromDB
    putStrLn "The Character is played by the following Actor:"
    putStrLn ""
    actor <- fetchAllValuesActor parseInt
    printActor $ head actor


-- |This functions queries the character table to fetch info about all the characters with a particular house chosen by the user
searchByCharacterHouse::IO [()]
searchByCharacterHouse = do
  putStrLn "*************************************************************************************************"
  putStrLn "*                  Enter a number to find all the Characters with that House                    *"
  putStrLn "*                                                                                               *"
  putStrLn "*   1 > Gryffindor          2 > Slytherin            3 > Hufflepuff       4 > Ravenclaw         *"
  putStrLn "*************************************************************************************************"
  option <- getLine
  if option == "1"
  then do characters <- fetchAllCharactersFromHouse "Gryffindor"
          mapM printCharacter characters
  else if option == "2"
  then do characters <- fetchAllCharactersFromHouse "Slytherin"
          mapM printCharacter characters
  else if option == "3"
  then do characters <- fetchAllCharactersFromHouse "Hufflepuff"
          mapM printCharacter characters
  else if option == "4"
  then do characters <- fetchAllCharactersFromHouse "Ravenclaw"
          mapM printCharacter characters
  else do
    putStrLn "Invalid Choice. Please Try again..."
    putStrLn ""
    searchByCharacterHouse

-- |This functions queries the character table to fetch info about all the characters with a particular blood status chosen by the user
searchByCharacterBloodStatus::IO [()]
searchByCharacterBloodStatus = do
  putStrLn "***********************************************************************************************"
  putStrLn "*             Enter a number to find all the Characters with that blood Status                *"
  putStrLn "*                                                                                             *"
  putStrLn "*           1 > Half Blood             2 > Pure Blood            3 > Muggle Born              *"
  putStrLn "***********************************************************************************************"
  option <- getLine
  if option == "1"
  then do characters <- fetchAllCharactersFromBloodStatus "Half-blood"
          mapM printCharacter characters
  else if option == "2"
  then do characters <- fetchAllCharactersFromBloodStatus "Pure-blood"
          mapM printCharacter characters
  else if option == "3"
  then do characters <- fetchAllCharactersFromBloodStatus "Muggle-born"
          mapM printCharacter characters
  else do
    putStrLn "Invalid Choice. Please Try again"
    putStrLn ""
    searchByCharacterBloodStatus

-- |This function provides the user with options to query the database in a particular way.
-- It's an interactive user interface that only terminates if the user press 0 and Enter
userMenu :: IO ()
userMenu = do
  putStrLn "************************************************************************************************************************************************"
  putStrLn "*                                              Welcome to the Harry Potter World                                                               *"
  putStrLn "*                                              -----XXXXXXXXXXXXXXXXXXXXXX------                                                               *"
  putStrLn "*                                   Enter a number to choose one of the options and press Enter                                                *"
  putStrLn "*                                                      Press 0 to quit                                                                         *"
  putStrLn "*                                                                                                                                              *"
  putStrLn "*    1 > Search by Actor Name    2 > Search by Character Name    3 > Search by Character blood Status    4 > Search by Character House          *"
  putStrLn "************************************************************************************************************************************************"
  option <- getLine
  if (option == "0")
  then do putStrLn ""
          putStrLn "Thank You for using our Application!!!"
          putStrLn ""
          return ()
  else if (option == "1")
  then do searchByActorName
          userMenu
  else if (option == "2")
  then do searchByCharacterName
          userMenu
  else if (option == "3")
  then do searchByCharacterBloodStatus
          userMenu
  else if (option == "4")
  then do searchByCharacterHouse
          userMenu
  else do
    putStrLn "Invalid Choice, Please Try Again...."
    userMenu
