{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Tags where

import Prelude hiding (lookup)
import Data.Bson
import Database.MongoDB

import Control.Monad.IO.Class
import Data.Aeson (ToJSON(..), encode, (.=), object)
import qualified Data.Aeson

import Data.List (elemIndex, sortBy)

-- Tag --
data Tag = Tag {
    source :: String,
    title :: String,
    url :: String
} deriving (Eq, Show, Read)

instance ToJSON Tag where
    toJSON (Tag source title url) = object ["source" .= source, "title" .= title, "url" .= url]

tagToDocument :: Tag -> Document
tagToDocument (Tag source title url) = ["_id" =: url, "source" =: source, "title" =: title, "url" =: url]

tagFromDocument :: Document -> Tag
tagFromDocument fs = Tag source title url
    where source = stringValue $ valueAt "source" fs
          title = stringValue $ valueAt "title" fs
          url = stringValue $ valueAt "url" fs



-- SearchTerm --
data SearchTerm = SearchTerm {
    searchTerm :: String,
    searchScore :: Int, 
    searchUrl :: String
} deriving (Eq, Show, Read)

termToDocument :: SearchTerm -> Document
termToDocument (SearchTerm term score url) = ["term" =: term, "score" =: score, "url" =: url]

termFromDocument :: Document -> SearchTerm
termFromDocument fs = SearchTerm term score url
    where term = stringValue $ valueAt "term" fs
          score = 10
          url = stringValue $ valueAt "url" fs

instance ToJSON SearchTerm where
    toJSON (SearchTerm term score url) = object ["term" .= term, "score" .= score, "url" .= url]






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


-- Manual Search Terms --
-- returns in score order -- 

findManual :: UString -> Action IO [Tag]
findManual term = do
    termDocs <- find (select ["term" =: term] "manualTerms") {project = ["url" =: 1, "_id" =: 0], sort = ["score" =: (-1)]} >>= rest
    let terms = map termFromDocument termDocs
    let urls = map searchUrl terms
    tagDocs <- find (select ["url" =: ["$in" =: urls]] "raw") >>= rest
    let tags = map tagFromDocument tagDocs
    let urlOrder a b = compare (elemIndex (url a) urls) (elemIndex (url b) urls)
    return $ sortBy urlOrder tags
    







-- Helpers --

runTags :: (MonadIO m) => Pipe -> Action m a -> m (Either Failure a)
runTags pipe actions = do
    e <- access pipe master "testtags" actions
    return e

-- liftIO makes it spin forever here, but not if you put in the context you're using it in
-- runTags :: MonadIO m => Pipe -> Action IO a -> m (Either Failure a)
-- runTags pipe actions = liftIO $ runTags pipe actions

connectTagsDb = runIOE $ connect (host "127.0.0.1")

populateMockData :: Action IO ()
populateMockData = do

    delete $ select [] "raw"
    insertMany_ "raw" $ map tagToDocument mockGaga
    
    delete $ select [] "manualTerms"
    insertMany_ "manualTerms" $ map termToDocument mockSearchTerms

    return ()


-- main :: IO ()
-- main = do
--     pipe <- connectTagsDb
--     result <- runTags pipe populateMockData
--     print result
-- 
--     result <- runTags pipe $ findManual "lady gaga"
--     print result
--     -- runTags pipe populateMockData   
--     return ()


mockSearchTerms = [ SearchTerm "lady gaga" 100 "http://en.wikipedia.org/wiki/Lady_gaga"
                  , SearchTerm "lady gaga" 80 "http://www.theinsider.com/music/45833_Bad_Romance_Named_All_Time_100_Songs/index.html"
                  , SearchTerm "lady gaga" 60 "http://en.wikipedia.org/wiki/Lady_Gaga_discography"
                  , SearchTerm "lady gaga" 40 "http://www.aoltv.com/2011/09/13/lady-gaga-madonna-inspiration-gaga-by-gaultier-video/"
                  , SearchTerm "lady" 100 "http://en.wikipedia.org/wiki/Lady"
                  ]


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
           , Tag "wikipedia" "Lady" "http://en.wikipedia.org/wiki/Lady"
           ]


















-- Some examples
-- findTags db term = db $ find (select ["title" =: term] "tags") >>= rest
-- findTagsGreater db term = db $ find (select ["title" =: ["$gte" =: term]] "tags") >>= rest
-- findTags db term = db $ find (select ["title" =: Regex term "i"] "tags") >>= rest

