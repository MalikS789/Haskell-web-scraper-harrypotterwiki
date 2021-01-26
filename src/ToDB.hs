-- |This Module contains function that talks to the database.
-- It contains functions that stores the parse data from website into an SQlite3 database using HDBC
-- It also contains functions that queries the database to get interesting information
module ToDB where

import CastScraper
import CharacterScraper
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.List


-- |This function initialises the cast table if doesn't exist
-- The table has 6 columns with the cast_id being the primary KEY
-- It relates to the character table with the role of the actors
initialiseCastDB :: Connection -> IO ()
initialiseCastDB conn =
  do
    run conn "CREATE TABLE IF NOT EXISTS cast (\
            \cast_id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \name VARCHAR(50) NOT NULL, \
            \dob VARCHAR(50) NOT NULL, \
            \nationality VARCHAR(50) NOT NULL, \
            \cast_gender VARCHAR(50) NOT NULL, \
            \role VARCHAR(50) NOT NULL\
            \)"
            []
    commit conn

-- |This function initialises the character table if doesn't exist
-- The table has 6 columns with the character_id being the primary KEY
initialiseCharacterDB :: Connection -> IO ()
initialiseCharacterDB conn =
  do
    run conn "CREATE TABLE IF NOT EXISTS character (\
            \character_id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \fname VARCHAR(50) NOT NULL, \
            \gender VARCHAR(50) NOT NULL, \
            \bloodStatus VARCHAR(50) NOT NULL, \
            \house VARCHAR(50) NOT NULL, \
            \patronus VARCHAR(50) NOT NULL\
            \)"
            []
    commit conn

-- |CharacterFromDB is a different type than the data type Character that is imported from CharacterScraper.
-- The Character data type from CharacterScraper doesn't have a unique ID
-- CharacterFromDB has an id function
data CharacterFromDB = CharacterDB {

  id::Integer,
  fname::String,
  gender::String,
  bloodStatus::String,
  house::String,
  patronus::String

} deriving (Show)


-- |ActorFromDB is a different type than the data type Actor that is imported from CastScraper.
-- The Actor data type from CastScraper doesn't have a unique ID
-- ActorFromDB has an id function
data ActorFromDB = ActorDB {

 cast_id::Integer,
 name::String,
 dob::String,
 nationality::String,
 cast_gender::String,
 role::String

} deriving (Show)


-- |This function converts Character (imported from CharacterScraper) to list of Sql Values
characterToSqlValues :: Character -> [SqlValue]
characterToSqlValues (Character fname gender bloodStatus house patronus)
    = [toSql fname, toSql gender, toSql bloodStatus, toSql house, toSql patronus]


-- |This function converts list of Sql values to CharacterFromDB data type
sqlValuesToCharacter :: [SqlValue] -> CharacterFromDB
sqlValuesToCharacter [id,fname,gender,bloodStatus,house,patronus]
    = CharacterDB (fromSql id) (fromSql fname) (fromSql gender) (fromSql bloodStatus) (fromSql house) (fromSql patronus)


-- |This function simply prepares an insert statement for the character table
prepareInsertCharacterIOStmt :: Connection -> IO Statement
prepareInsertCharacterIOStmt conn = prepare conn "INSERT INTO character (fname, gender, bloodStatus, house, patronus) VALUES (?,?,?,?,?)"


-- |This function inserts list of Characters parsed from the website into the database
batchInsertCharacter::[Character]-> IO ()
batchInsertCharacter characters = do
  conn <- connectSqlite3 "HarryPotterDB.db"
  initialiseCharacterDB conn
  stmt <- prepareInsertCharacterIOStmt conn
  executeMany stmt (map characterToSqlValues characters)
  commit conn


-- |This function uses a SELECT sql statement to query all the data from the character table using a particular id
fetchAllValuesCharacter :: Int -> IO [CharacterFromDB]
fetchAllValuesCharacter char_id = do
    conn <- connectSqlite3 "HarryPotterDB.db"
    res <- quickQuery' conn "SELECT * FROM character WHERE character_id=?" [toSql (char_id :: Int)]
    return (map sqlValuesToCharacter res)

-- |This function takes a character id as an input argument and returns true if the id doesn't exist in the database
characterNotInDB :: Int -> IO Bool
characterNotInDB character_id = do
    conn <- connectSqlite3 "HarryPotterDB.db"
    res <- quickQuery' conn "SELECT character_id FROM character WHERE character_id=?" [toSql (character_id::Int)]
    return (length res == 0)

-- |This function uses a SELECT sql statement to query all the data from the character table with a particular House
fetchAllCharactersFromHouse :: String -> IO [CharacterFromDB]
fetchAllCharactersFromHouse char_house = do
    conn <- connectSqlite3 "HarryPotterDB.db"
    res <- quickQuery' conn "SELECT * FROM character WHERE house=?" [toSql (char_house :: String)]
    return (map sqlValuesToCharacter res)

-- |This function uses a SELECT sql statement to query all the data from the character table with a particular Blood Status
fetchAllCharactersFromBloodStatus :: String -> IO [CharacterFromDB]
fetchAllCharactersFromBloodStatus char_blood = do
    conn <- connectSqlite3 "HarryPotterDB.db"
    res <- quickQuery' conn "SELECT * FROM character WHERE bloodStatus=?" [toSql (char_blood :: String)]
    return (map sqlValuesToCharacter res)


-- |This function converts Actor (imported from CastScraper) to list of Sql Values
actorToSqlValues :: Actor -> [SqlValue]
actorToSqlValues (Actor name dob nationality cast_gender role)
      = [toSql name, toSql dob, toSql nationality, toSql cast_gender, toSql role]


-- |This function converts list of Sql values to ActorFromDB data type
sqlValuesToActor :: [SqlValue] -> ActorFromDB
sqlValuesToActor [cast_id,name,dob,nationality,cast_gender,role]
    = ActorDB (fromSql cast_id) (fromSql name)  (fromSql dob) (fromSql nationality) (fromSql cast_gender) (fromSql role)


-- |This function simply prepares an insert statement for the character table
prepareInsertActorIOStmt:: Connection -> IO Statement
prepareInsertActorIOStmt conn = prepare conn "INSERT INTO cast (name, dob, nationality, cast_gender, role) VALUES (?,?,?,?,?)"


-- |This function inserts list of Actors parsed from the website into the database
batchInsertActor::[Actor]-> IO ()
batchInsertActor actors = do
     conn <- connectSqlite3 "HarryPotterDB.db"
     conn <- connectSqlite3 "HarryPotterDB.db"
     initialiseCastDB conn
     stmt <- prepareInsertActorIOStmt conn
     executeMany stmt (map actorToSqlValues actors)
     commit conn


-- |This function takes a cast id as an input argument and returns true if the id doesn't exist in the database
actorNotInDB :: Int -> IO Bool
actorNotInDB cast_id = do
    conn <- connectSqlite3 "HarryPotterDB.db"
    res <- quickQuery' conn "SELECT cast_id FROM cast WHERE cast_id=?" [toSql (cast_id::Int)]
    return (length res == 0)


-- |This function uses a SELECT sql statement to query all the data from the actor table using a particular id
fetchAllValuesActor :: Int -> IO [ActorFromDB]
fetchAllValuesActor cast_id = do
    conn <- connectSqlite3 "HarryPotterDB.db"
    res <- quickQuery' conn "SELECT * FROM cast WHERE cast_id=?" [toSql (cast_id :: Int)]
    return (map sqlValuesToActor res)
