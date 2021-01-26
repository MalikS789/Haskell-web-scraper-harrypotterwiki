-- |This Module performs http requests to get the HTML in return. It then parses the HTML to extract Character Info
module CharacterScraper where

import Network.HTTP.Simple -- http-conduit
import qualified Data.ByteString.Lazy.Char8 as B --bytestring
import Control.Monad
import Text.HTML.TagSoup --tagsoup

type CharacterName = String -- CharacterName format: "firstName_LastName". eg. "Harry_Potter"
type URL = String

data Character = Character {

  fname::String,
  gender::String,
  bloodStatus::String,
  house::String,
  patronus::String

} deriving (Show)


-- |This function builds the url using the CharacterName in the correct format.
-- This helps render Actor info dynamically.
-- It returns the url to be scraped and parsed.
urlBuilder :: CharacterName -> URL
urlBuilder name = "https://harrypotter.fandom.com/wiki/" ++ name


-- |This function uses the tagsoup module to parse the requested HTML to a list of tags.
-- it takes in the url as a string
-- it returns a list of tags in the form, eg. "<hello>my&amp;</world>" == [TagOpen "hello" [],TagText "my&",TagClose "world"]
getTags :: URL -> IO [Tag String]
getTags url = do
  req <- parseRequest $ url
  res <- httpLBS req
  let tags = parseTags $ B.unpack (getResponseBody res)
  return $ tags


-- |This function traverses through the html(parsed as a list of tags) and extracts the Character's full name
getFullName :: CharacterName -> IO String
getFullName name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<h2 data-source=name>") tags
  let name = innerText data'
  return $ unwords $ takeWhile (/="Biographical") $ words name


-- |This function traverses through the html(parsed as a list of tags) and extracts the Character's date of gender
getGender :: CharacterName -> IO String
getGender name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=gender>") tags
  let gender = innerText data'
  return $ takeWhile (/='[') $ (words gender) !! 1


-- |This function traverses through the html(parsed as a list of tags) and extracts the Character's blood Status, eg. "Half-Blood"
getBloodStatus :: CharacterName -> IO String
getBloodStatus name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=blood>") tags
  let status = innerText data'
  return $ takeWhile(/='[') $ (words status) !! 2

-- |This function traverses through the html(parsed as a list of tags) and extracts the Character's House, eg. "Gryffindor"
getHouse :: CharacterName -> IO String
getHouse name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=house>") tags
  let house = innerText data'
  return $ takeWhile(/='[') $ (words house) !! 1

-- |This function traverses through the html(parsed as a list of tags) and extracts the Character's patronus, eg. "stag"
getPatronus :: CharacterName -> IO String
getPatronus name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~== "<div data-source=patronus>") tags
  let patronus = innerText data'
  return $ takeWhile(/='[') $ (words patronus) !! 1


-- |This function takes all the extracted info from the parsed HTML and creates a new Character record
-- The function takes in an Character name as argument in the format "firstName_LastName". eg. "Harry_Potter"
-- it returns an Character
newCharacter :: CharacterName -> IO Character
newCharacter name = do
  fname <- getFullName name
  gender <- getGender name
  bloodStatus <- getBloodStatus name
  house <- getHouse name
  patronus <- getPatronus name
  let newCharacter = Character fname gender bloodStatus house patronus
  return  newCharacter
