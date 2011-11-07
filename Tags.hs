{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

{-
    This is the file where all the magic happens. 
        Has Tag, which I define some converters for

        Also has several different ways to store and access tags. 

-}

module Tags where

import Prelude hiding (lookup)
import Data.Bson
import Database.MongoDB

import Control.Monad.IO.Class
import Data.Aeson (ToJSON(..), encode, (.=), object)
import qualified Data.Aeson

import qualified Data.CompactString as CS

import Data.List (elemIndex, sortBy, init, nub)
import qualified Data.List as List

import Data.Char (toLower)
import Data.Ratio 

import NLP.Stemmer 

-- Tag: for simplicity, I didn't make different data types for the different experiments, so this type is a little bloated --
data Tag = Tag {
    source :: String,
    title :: String,
    url :: String,
    term :: String,
    score :: Int,
    terms :: [String]
} deriving (Eq, Show, Read)

-- This allows us to call toJSON on a Tag --
instance ToJSON Tag where
    toJSON (Tag source title url term score terms) = object ["source" .= source, "title" .= title, "url" .= url, "score" .= score, "term" .= term, "terms" .= terms]

-- This allows us to call sort on [Tag]
instance Ord Tag where
    compare x y = compare (score x) (score y)

-- convert to mongo Document
tagToDocument :: Tag -> Document
tagToDocument (Tag source title url term score terms) = ["_id" =: (term ++ "-" ++ url), "source" =: source, "title" =: title, "url" =: url, "score" =: score, "term" =: term, "terms" =: terms]

-- convert from mongo Document
tagFromDocument :: Document -> Tag
tagFromDocument fs = Tag source title url term score terms
    where source = sval "source" fs
          title = sval "title" fs
          url = sval "url" fs
          term = sval "term" fs
          score = ival $ valueAt "score" fs
          terms = aval $ valueAt "terms" fs
          sval field doc = stringValue $ valueAt field doc
          aval (Array vs) = map stringValue vs
          aval _ = []
          ival (Int32 i) = fromIntegral i
          ival _ = 0

          stringValue :: Value -> String
          stringValue (Data.Bson.String s) = Data.Bson.unpack s
          stringValue _ = ""


-- sets the term and the score for that term
setTagTermScore :: String -> Int -> Tag -> Tag
setTagTermScore term score tag = Tag (source tag) (title tag) (url tag) (normalizeTerm term) score (terms tag)

-- sets just the score
setTagScore :: Int -> Tag -> Tag
setTagScore score tag = Tag (source tag) (title tag) (url tag) (term tag) score (terms tag)

-- doesn't have a term or score
simpleTag :: String -> String -> String -> Tag
simpleTag source title url = Tag source title url "" 0 []






-- Raw Tags :: I'm just matching against the title here --
rawFind :: UString -> Action IO [Document]
rawFind term = find (select ["title" =: Regex term "i"] "raw") >>= rest  -- this is equivalent to cursor <- FIRST; result <- rest cursor

rawInsert :: [Document] -> Action IO [Value]
rawInsert tags = do
    delete $ select [] "raw"
    insertMany "raw" tags








-- Manual Search Terms --
-- returns in score order -- 

termScoreCursor term coll = find (select ["term" =: term] coll) {project = ["_id" =: 0], sort = ["score" =: (-1)]}  

findManual :: UString -> Action IO [Tag]
findManual term = do
    tagDocs <- termScoreCursor term "raw" >>= rest
    return $ map tagFromDocument tagDocs
    


-- Source Weighted Terms --
-- changed to multiplication -- ratio weighting -- ratios are like fractions. Uses the % operator
-- whatever :) addition probably works just as well ;)
sourceScoreTag :: Tag -> Tag
sourceScoreTag tag = setTagScore (round (tagScore `multRatios` (sourceScore tagSource))) tag
    where sourceScore "theinsider" = 160 % 100
          sourceScore "brief" = 180 % 100
          sourceScore _ = 100 % 100 
          tagSource = source tag
          tagScore = score tag % 1

-- multiplies two ratios. I didn't find this after a quick search, so I just wrote it
multRatios :: (Num a, Integral a) => Ratio a -> Ratio a -> Ratio a
multRatios a b = (numerator a * numerator b) % (denominator a * denominator b)
        
-- uses the same term score cursor 
findSourceWeighted :: UString -> Action IO [Tag]
findSourceWeighted term = do
    tagDocs <- termScoreCursor term "sourceWeighted" >>= rest
    return $ map tagFromDocument tagDocs







-- Score Lazily --
-- In this section, I didn't save the lazily generated scores back to the database, but I could have

normalizeTerm :: String -> String
normalizeTerm term = map toLower term

-- I'm sure some of these functions could be more elegant, but at least they're pretty clear. 
termScoreTag :: String -> Tag -> Tag
termScoreTag term tag = setTagTermScore term termScore tag
    where termScore = round $ maxScore / distanceToTerm
          distanceToTerm = tagNumWords % termNumWords -- ignores if has fewer words, but we shouldn't get those
          maxScore = 100
          numWords text = (length.words) text
          termNumWords = numWords term
          tagNumWords = numWords (title tag)

-- Finds term scores only, without source weighted applied. 
-- imported manually for lady gaga
findTermScored :: String -> Action IO [Tag]
findTermScored term = do
    tagDocs <- termScoreCursor (normalizeTerm term) "termScored" >>= rest
    return $ map tagFromDocument tagDocs 

-- Finds any title matching the term, then scores it
generateScoresForTerm :: String -> Action IO [Tag]
generateScoresForTerm term = do
    let cursor = find (select ["title" =: Regex (CS.pack (normalizeTerm term)) "i"] "raw")  {project = ["_id" =: 0], sort = ["score" =: (-1)]}  
    tagDocs <- cursor >>= rest
    let tags = reverse $ List.sort $ map ((termScoreTag term).tagFromDocument) tagDocs 
    return tags

lazyFindTermScored :: String -> Action IO [Tag]
lazyFindTermScored term = do
    tags <- generateScoresForTerm term -- I could save them, but that's just an optimization
    return tags


    

findFullyScored :: String -> Action IO [Tag]
findFullyScored term = do
    tags <- generateScoresForTerm term
    return $ reverse $ List.sort $ map sourceScoreTag tags





-- Multiple Keyword Docs --
-- splitEachKeyword :: Tag -> [Tag]
-- splitEachKeyword tag = map getTag $ nub.words $ title tag
--     where getTag word = Tag (source tag) (title tag) (url tag) (normalizeTerm word) 0 
-- 
-- 
-- ownTermScoreTag tag = termScoreTag (term tag) tag
-- 
-- populateMultiple :: Action IO ()
-- populateMultiple = do
--     delete $ select [] "multiple"
--     let tags = concat $ map splitEachKeyword manualGaga
--     insertMany_ "multiple" $ map (tagToDocument.ownTermScoreTag) tags
--     return ()
-- 
-- I think this is too hard
-- findMultiple :: String -> Action IO [Tag]
-- findMultiple term = do
    -- let terms = words term
    -- find (select ["term" =: "i"] "raw")  {project = ["_id" =: 0], sort = ["score" =: (-1)]}  
    -- wait, is this even possible? 
    -- lady 50
    -- gaga 50
    -- the score definitely doesn't matter
    -- well, I want the cross-section of terms. Things that have the same url, but different terms. 
    -- that kind of query is hard in mongo. Try the other way






-- Multiple Keyword Array --


keywords :: String -> [String]
keywords title = map normalizeTerm $ stemWords English $ words title
-- keywords title = map normalizeTerm $ words title

keywordify :: Tag -> Tag
keywordify (Tag source title url term score terms) = Tag source title url term score (keywords title)    
    
populateMultiple :: Action IO ()
populateMultiple = do
    delete $ select [] "multiple"
    let tags = map keywordify manualGaga
    insertMany_ "multiple" $ map tagToDocument tags
    return ()


findMultiple :: String -> Action IO [Tag]
findMultiple term = do
    let terms = keywords term
    let cursor = find (select ["terms" =: ["$all" =: terms]] "multiple")  {project = ["_id" =: 0]}  
    tagDocs <- cursor >>= rest
    liftIO $ print $ tagDocs
    return $ reverse $ List.sort $ map (sourceScoreTag.(scoreTagByTerms term).tagFromDocument) tagDocs

scoreTagByTerms :: String -> Tag -> Tag
scoreTagByTerms term tag = setTagTermScore term termScore tag
    where termScore = round $ maxScore / distanceToTerm
          distanceToTerm = tagNumWords % termNumWords -- ignores if has fewer words, but we shouldn't get those
          maxScore = 100
          numWords text = (length.words) text
          termNumWords = max 1 $ length $ keywords term 
          tagNumWords = max 1 $ length (terms tag)
    
    






















-- Helpers --

-- Just runs an Action function against our database (so it can magically be passed the db connection)
runTags :: (MonadIO m) => Pipe -> Action m a -> m (Either Failure a)
runTags pipe actions = do
    e <- access pipe master "testtags" actions
    return e

-- liftIO makes it spin forever here, but not if you put in the context you're using it in
-- runTags :: MonadIO m => Pipe -> Action IO a -> m (Either Failure a)
-- runTags pipe actions = liftIO $ runTags pipe actions

-- returns the Pipe for the db
connectTagsDb = runIOE $ connect (host "127.0.0.1")


-- Populates the databases for the different experiments
populateMockData :: Action IO ()
populateMockData = do

    delete $ select [] "raw"
    insertMany_ "raw" $ map tagToDocument manualGaga
    
    delete $ select [] "sourceWeighted"
    insertMany_ "sourceWeighted" $ map (tagToDocument . sourceScoreTag) manualGaga

    delete $ select [] "termScored"

    lgtags <- generateScoresForTerm "Lady Gaga"
    ltags <- generateScoresForTerm "Lady"

    insertMany_ "termScored" $ map tagToDocument lgtags
    insertMany_ "termScored" $ map tagToDocument ltags

    populateMultiple

    -- res <- findMultiple "gaga lady"
    -- liftIO $ print res
    
    -- results <- findTermScored "Lady Gaga"
    -- liftIO $ print results
    -- 
    -- results <- findTermScored "Lady"
    -- liftIO $ print results

    return ()

-- I just put an "a" in front of main to disable it
amain = do
    
    pipe <- connectTagsDb
    result <- runTags pipe populateMockData
    print result

    -- result <- runTags pipe $ findManual "lady gaga"
    -- print result

    return ()




-- MOCK LADY GAGA DATA --
-- manual term scores, based on how many words it has. Exact = 200, +1 = 150, mid = 100, low = 50
manualGaga = [ Tag "wikipedia" "Lady Gaga" "http://en.wikipedia.org/wiki/Lady_gaga" "lady gaga" 200 []
           , Tag "wikipedia" "Lady Gaga Presents the Monster Ball Tour" "http://en.wikipedia.org/wiki/Lady_Gaga_Presents_the_Monster_Ball_Tour:_At_Madison_Square_Garden" "lady gaga" 100 []
           , Tag "wikipedia" "Lady Gaga discography" "http://en.wikipedia.org/wiki/Lady_Gaga_discography" "lady gaga" 150 []
           , Tag "wikipedia" "Lady Gaga Queen of Pop" "http://en.wikipedia.org/wiki/Lady_Gaga:_Queen_of_Pop" "lady gaga" 100 []
           , Tag "wikipedia" "Lady Gaga World Tour 2010" "http://en.wikipedia.org/wiki/Lady_Gaga_World_Tour_2010" "lady gaga" 100 []
           , Tag "wikipedia" "Lady Gaga the fame monster" "http://en.wikipedia.org/wiki/Lady_Gaga_the_fame_monster" "lady gaga" 100 []
           , Tag "wikipedia" "Lady Gaga Telephone" "http://en.wikipedia.org/wiki/Lady_Gaga_Telephone" "lady gaga" 100 []
           , Tag "wikipedia" "Lady Gaga x Terry Richardson" "http://en.wikipedia.org/wiki/Lady_Gaga_x_Terry_Richardson" "lady gaga" 100 []
           , Tag "wikipedia" "Lady Gaga Revenge" "http://en.wikipedia.org/wiki/Lady_Gaga_Revenge#Lady_Gaga_Revenge" "lady gaga" 150 []
           , Tag "wikipedia" "Lady gaga You and I" "http://en.wikipedia.org/wiki/Lady_gaga_you_and_i" "lady gaga" 100 []

           , Tag "theinsider" "Lady Gaga's 'Bad Romance' Enters All-Time 100" "http://www.theinsider.com/music/45833_Bad_Romance_Named_All_Time_100_Songs/index.html" "lady gaga" 50 []

           , Tag "aoltv" "Jerry Springer Dresses Up as Lady Gaga for Halloween (VIDEO)" "http://www.aoltv.com/2011/10/31/jerry-springer-lady-gaga-halloween-video/" "lady gaga" 50 []
           , Tag "aoltv" "Lady Gaga Talks About Madonna as Inspiration on 'Gaga by Gaultier' (VIDEO)" "http://www.aoltv.com/2011/09/13/lady-gaga-madonna-inspiration-gaga-by-gaultier-video/" "lady gaga" 50 []
           , Tag "aoltv" "Lady Gaga, Britney Spears, Beyonce and Cloris Leachman Highlight the 2011 VMAs (VIDEO)" "http://www.aoltv.com/2011/08/29/women-win-big-vmas-highlights-list-of-winners-video/" "lady gaga" 50 []
           , Tag "aoltv" "Lady Gaga Lends Voice to 'The Simpsons,' Andre Braugher Heading to 'SVU' and More Casting News" "http://www.aoltv.com/2011/08/23/lady-gaga-the-simpsons/" "lady gaga" 50 []

           , Tag "brief" "Lady Gaga" "http://tvtag.i.tv/briefs/LadyGaga" "lady gaga" 200 []

           , Tag "wikipedia" "Lady" "http://en.wikipedia.org/wiki/Lady" "lady" 200 []
           ]


















-- Some examples
-- findTags db term = db $ find (select ["title" =: term] "tags") >>= rest
-- findTagsGreater db term = db $ find (select ["title" =: ["$gte" =: term]] "tags") >>= rest
-- findTags db term = db $ find (select ["title" =: Regex term "i"] "tags") >>= rest

