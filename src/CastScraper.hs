-- |This Module performs http requests to get the HTML in return. It then parses the HTML to extract Actor Info
module CastScraper where

import Network.HTTP.Simple -- http-conduit
import qualified Data.ByteString.Lazy.Char8 as B --bytestring
import Control.Monad
import Text.HTML.TagSoup --tagsoup

type ActorName = String -- ActorName format: "firstName_LastName". eg. "Emma_Watson"
type URL = String

data Actor = Actor {

 name::String,
 dob::String,
 nationality::String,
 cast_gender::String,
 role::String

} deriving (Show)


-- |This function builds the url using the ActorName in the correct format.
-- This helps render Actor info dynamically.
-- It returns the url to be scraped and parsed.
urlBuilder :: ActorName -> URL
urlBuilder name = "https://harrypotter.fandom.com/wiki/" ++ name


-- |This function uses the tagsoup module to parse the requested HTML to a list of tags.
-- it takes in the url as a string
-- it returns a list of tags in the form, eg. "<hello>my&amp;</world>" == [TagOpen "hello" [],TagText "my&",TagClose "world"]
getTags :: URL -> IO [Tag String]
getTags url = do
  req <- parseRequest $ url
  res <- httpLBS req  -- gets the html from the url as ByteString
  let tags = parseTags $ B.unpack (getResponseBody res) -- first coverts ByteString to String and then to list of tags
  return $ tags


-- |This function traverses through the html(parsed as a list of tags) and extracts the actor's name
getActorName :: ActorName -> IO String
getActorName name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~=="<h2 data-source=name>") tags
  let fname = innerText data'
  return $ unwords $ takeWhile (/="Biographical") $ words fname


-- |This function traverses through the html(parsed as a list of tags) and extracts the actor's date of birth
getActorDOB :: ActorName -> IO String
getActorDOB name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~=="<div data-source=born>") tags
  let dob = innerText data'
  return $ (words dob) !! 1 ++ " " ++ (words dob) !! 2 ++ (words dob) !! 3


-- |This function traverses through the html(parsed as a list of tags) and extracts the actor's date of nationality
getActorNationality :: ActorName -> IO String
getActorNationality name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~=="<div data-source=nationality>") tags
  let nationality = innerText data'
  return $ (words nationality) !! 1


-- |This function traverses through the html(parsed as a list of tags) and extracts the actor's date of gender
getActorGender :: ActorName -> IO String
getActorGender name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~=="<div data-source=gender>") tags
  let gender = innerText data'
  return $ (words gender) !! 1


-- |This function traverses through the html(parsed as a list of tags) and extracts the actor's role in harry potter
getActorRole :: ActorName -> IO String
getActorRole name = do
  tags <- getTags $ urlBuilder name
  let data' = head $ sections (~=="<div data-source=harrypotterrole>") tags
  let role = innerText data'
  return $ (words role) !! 3 ++ " " ++ (words role) !! 4



-- |This function takes all the extracted info from the parsed HTML and creates a new actor record
-- The function takes in an actor name as argument in the format "firstName_LastName". eg. "Emma_Watson"
-- it returns an Actor
getActorInfo :: ActorName -> IO Actor
getActorInfo name = do
  fname <- getActorName name
  dob <- getActorDOB name
  nationality <- getActorNationality name
  gender <- getActorGender name
  role <- getActorRole name
  let newActor = Actor fname dob nationality gender role
  return  newActor
