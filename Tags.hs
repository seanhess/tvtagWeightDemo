{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Tags where

import Prelude hiding (lookup)
import Data.Bson
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.IO.Control
import Database.MongoDB

import Mongo

-- Better yet, can I make this function return type: Action m a -> m (Either Failure a)
-- I need to know more first
-- insertTags :: MonadIO m => (Action m () -> t) -> [Document] -> t
-- insertTags db tags = db $ insertMany_ "tags" tags

-- findTags db term = db $ find (select ["title" =: term] "tags") >>= rest
-- findTagsGreater db term = db $ find (select ["title" =: ["$gte" =: term]] "tags") >>= rest
-- findTags db term = db $ find (select ["title" =: Regex term "i"] "tags") >>= rest

type SearchTerm = UString

data Tag = Tag {
    source :: String,
    title :: String,
    url :: String
} deriving (Eq, Show, Read)

rawDocument :: Tag -> Document
rawDocument tag = ["_id" =: (url tag), "source" =: (source tag), "title" =: (title tag), "url" =: (url tag)]

-- Raw Tags --
rawFind   db term = db $ find (select ["title" =: Regex term "i"] "raw") >>= rest 
rawInsert db tags = do
    db $ delete $ select [] "raw"
    db $ insertMany_ "raw" tags

-- Manual Scoring --
manualScoreFind db term = db $ find (select ["title" =: Regex term "i"] "manual") >>= rest 
manualScoreInsert db tags = do
    db $ delete $ select [] "manual"
    db $ insertMany_ "manual" tags

initTests db = do
    rawInsert db $ map rawDocument mockGaga
    manualScoreInsert db $ map rawDocument mockGaga

main = do
    pipe <- mconnect "127.0.0.1"
    let db = mdb pipe

    -- initialize
    initTests db

    Right docs <- rawFind db "Lady Gaga"
    print docs

    print "DONE"




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




































