{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Tags where

import Prelude hiding (lookup)
import Data.Bson
import Database.MongoDB

import Control.Monad.IO.Class
import Data.Aeson (ToJSON(..), encode, (.=), object)
import qualified Data.Aeson

-- My Data Type
data Tag = Tag {
    source :: String,
    title :: String,
    url :: String
} deriving (Eq, Show, Read)

instance ToJSON Tag where
    toJSON (Tag source title url) = object ["source" .= source, "title" .= title, "url" .= url]

toDocument :: Tag -> Document
toDocument (Tag source title url) = ["_id" =: url, "source" =: source, "title" =: title, "url" =: url]

-- Convert a tag document to a tag
fromDocument :: Document -> Tag
fromDocument fs = Tag source title url
    where source = stringValue $ valueAt "source" fs
          title = stringValue $ valueAt "title" fs
          url = stringValue $ valueAt "url" fs

stringValue :: Value -> String
stringValue (Data.Bson.String s) = Data.Bson.unpack s
stringValue _ = ""

-- Raw Tags --
rawFind :: UString -> Action IO [Document]
rawFind term = find (select ["title" =: Regex term "i"] "raw") >>= rest 

rawInsert :: [Document] -> Action IO [Value]
rawInsert tags = do
    delete $ select [] "raw"
    insertMany "raw" tags

-- Manual Scoring --
manualScoreFind term = find (select ["title" =: Regex term "i"] "manual") >>= rest 
manualScoreInsert tags = do
    delete $ select [] "manual"
    insertMany_ "manual" tags

-- Runs our actions on a given pipe / db

runTagsInner :: (MonadIO m) => Pipe -> Action m a -> m (Either Failure a)
runTagsInner pipe actions = do
    e <- access pipe master "testtags" actions
    return e

runTags :: MonadIO m => Pipe -> Action IO a -> m (Either Failure a)
runTags pipe actions = liftIO $ runTags pipe actions

populateMockData :: Action IO ()
populateMockData = do
    let gagaDocs = map toDocument mockGaga
    rawInsert gagaDocs
    manualScoreInsert gagaDocs
    return ()

connectTagsDb = runIOE $ connect (host "127.0.0.1")
    
-- main :: IO ()
-- main = do
--     pipe <- connectTagsDb
--     runTags pipe populateMockData   
--     tags <- runTags pipe (rawFind "Lady Gaga")
--     print tags
--     return ()



-- MOCK LADY GAGA DATA --
mockGaga = [ Tag "wikipedia" "Lady Gaga" "http://en.wikipedia.org/wiki/Lady_gaga"
           , Tag "wikipedia" "Lady Gaga Presents the Monster Ball Tour" "http://en.wikipedia.org/wiki/Lady_Gaga_Presents_the_Monster_Ball_Tour:_At_Madison_Square_Garden"
           , Tag "wikipedia" "Lady Gaga discography" "http://en.wikipedia.org/wiki/Lady_Gaga_discography"
           , Tag "wikipedia" "Lady Gaga Queen of Pop" "http://en.wikipedia.org/wiki/Lady_Gaga:_Queen_of_Pop"
           , Tag "wikipedia" "Lady Gaga World Tour 2010" "http://en.wikipedia.org/wiki/Lady_Gaga_World_Tour_2010"
           , Tag "wikipedia" "Lady Gaga the fame monster" "http://en.wikipedia.org/wiki/Lady_Gaga_the_fame_monster"
           , Tag "wikipedia" "Lady Gaga Telephone" "http://en.wikipedia.org/wiki/Lady_Gaga_Telephone"
           , Tag "wikipedia" "Lady Gaga x Terry Richardson" "http://en.wikipedia.org/wiki/Lady_Gaga_x_Terry_Richardson"
           , Tag "wikipedia" "Lady Gaga Revenge" "http://en.wikipedia.org/wiki/Lady_Gaga_Revenge#Lady_Gaga_Revenge"
           , Tag "wikipedia" "Lady gaga You and I" "http://en.wikipedia.org/wiki/Lady_gaga_you_and_i"

           , Tag "theinsider" "Lady Gaga's 'Bad Romance' Enters All-Time 100" "http://www.theinsider.com/music/45833_Bad_Romance_Named_All_Time_100_Songs/index.html"

           , Tag "aoltv" "Jerry Springer Dresses Up as Lady Gaga for Halloween (VIDEO)" "http://www.aoltv.com/2011/10/31/jerry-springer-lady-gaga-halloween-video/"
           , Tag "aoltv" "Lady Gaga Talks About Madonna as Inspiration on 'Gaga by Gaultier' (VIDEO)" "http://www.aoltv.com/2011/09/13/lady-gaga-madonna-inspiration-gaga-by-gaultier-video/"
           , Tag "aoltv" "Lady Gaga, Britney Spears, Beyonce and Cloris Leachman Highlight the 2011 VMAs (VIDEO)" "http://www.aoltv.com/2011/08/29/women-win-big-vmas-highlights-list-of-winners-video/"
           , Tag "aoltv" "Lady Gaga Lends Voice to 'The Simpsons,' Andre Braugher Heading to 'SVU' and More Casting News" "http://www.aoltv.com/2011/08/23/lady-gaga-the-simpsons/"
           ]


















-- Some examples
-- findTags db term = db $ find (select ["title" =: term] "tags") >>= rest
-- findTagsGreater db term = db $ find (select ["title" =: ["$gte" =: term]] "tags") >>= rest
-- findTags db term = db $ find (select ["title" =: Regex term "i"] "tags") >>= rest

