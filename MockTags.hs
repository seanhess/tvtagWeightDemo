{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module MockTags where

import Database.MongoDB 
import Data.CompactString ()


mockTag :: String -> String -> String -> [Field]
mockTag source title url =  ["source" =: source, "title" =: title, "url" =: url]


mockGaga = [ mockTag "wikipedia" "Lady Gaga" "http://en.wikipedia.org/wiki/Lady_gaga"
           , mockTag "wikipedia" "Lady Gaga Presents the Monster Ball Tour" "http://en.wikipedia.org/wiki/Lady_Gaga_Presents_the_Monster_Ball_Tour:_At_Madison_Square_Garden"
           , mockTag "wikipedia" "Lady Gaga discography" "http://en.wikipedia.org/wiki/Lady_Gaga_discography"
           , mockTag "wikipedia" "Lady Gaga Queen of Pop" "http://en.wikipedia.org/wiki/Lady_Gaga:_Queen_of_Pop"
           , mockTag "wikipedia" "Lady Gaga World Tour 2010" "http://en.wikipedia.org/wiki/Lady_Gaga_World_Tour_2010"
           , mockTag "wikipedia" "Lady Gaga the fame monster" "http://en.wikipedia.org/wiki/Lady_Gaga_the_fame_monster"
           , mockTag "wikipedia" "Lady Gaga Telephone" "http://en.wikipedia.org/wiki/Lady_Gaga_Telephone"
           , mockTag "wikipedia" "Lady Gaga x Terry Richardson" "http://en.wikipedia.org/wiki/Lady_Gaga_x_Terry_Richardson"
           , mockTag "wikipedia" "Lady Gaga Revenge" "http://en.wikipedia.org/wiki/Lady_Gaga_Revenge#Lady_Gaga_Revenge"
           , mockTag "wikipedia" "Lady gaga You and I" "http://en.wikipedia.org/wiki/Lady_gaga_you_and_i"

           , mockTag "theinsider" "Lady Gaga's 'Bad Romance' Enters All-Time 100" "http://www.theinsider.com/music/45833_Bad_Romance_Named_All_Time_100_Songs/index.html"

           , mockTag "aoltv" "Jerry Springer Dresses Up as Lady Gaga for Halloween (VIDEO)" "http://www.aoltv.com/2011/10/31/jerry-springer-lady-gaga-halloween-video/"
           , mockTag "aoltv" "Lady Gaga Talks About Madonna as Inspiration on 'Gaga by Gaultier' (VIDEO)" "http://www.aoltv.com/2011/09/13/lady-gaga-madonna-inspiration-gaga-by-gaultier-video/"
           , mockTag "aoltv" "Lady Gaga, Britney Spears, Beyonce and Cloris Leachman Highlight the 2011 VMAs (VIDEO)" "http://www.aoltv.com/2011/08/29/women-win-big-vmas-highlights-list-of-winners-video/"
           , mockTag "aoltv" "Lady Gaga Lends Voice to 'The Simpsons,' Andre Braugher Heading to 'SVU' and More Casting News" "http://www.aoltv.com/2011/08/23/lady-gaga-the-simpsons/"
           ]

-- main = print "WORKS"

-- printIO :: (Show a) => IO a -> IO ()
-- printIO action = do
--     result <- action
--     print result
-- 
-- simpleExamples = do
--     pipe <- runIOE $ connect $ host "127.0.0.1"
--     let run act = access pipe master "tesths" act
-- 
--     -- remove everything
--     run $ delete $ select [] "posts"
-- 
--     -- insert a post --
--     let post = ["author" =: "Mike", "text" =: "My first blog Post!", "tags" =: ["mongoDB", "Haskell"]]
--     run $ insert "posts" post
-- 
--     -- see the collections --
--     run allCollections >>= print
-- 
--     -- find one
--     (run $ findOne $ select [] "posts") >>= print
--     (run $ findOne $ select ["author" =: "Mike"] "posts") >>= print
--     print =<< (run $ findOne $ select ["author" =: "Eliot"] "posts")
-- 
--     -- insert many
--     let post1 = ["author" =: "Mike", "text" =: "Another post!", "tags" =: ["bulk", "insert"]]
--     let post2 = ["author" =: "Eliot", "title" =: "MongoDB is fun", "text" =: "and pretty easy too!"]
--     run $ insertMany "posts" [post1, post2] 
-- 
--     -- find many -- you should close a cursor manually if you don't use it with next or rest
--     count <- run $ count $ select [] "posts"
--     print count
--     posts <- run $ find (select ["author" =: "Mike"] "posts") >>= rest
--     print posts
-- 
--     -- sort --
--     sortedPosts <- run $ rest =<< find (select [] "posts") {sort = ["author" =: 1, "text" =: 1]}
--     print sortedPosts
-- 
--     -- update --
--     run $ fetch (select ["author" =: "Eliot"] "posts") >>= save "posts" . merge ["tags" =: ["hello"]]
--     run $ save "posts" ["author" =: "Tony", "text" =: "hello world"]
--     run $ modify (select [] "posts") ["$push" =: ["tags" =: "new"]]
-- 
--     -- can I partially apply posts? --
--     let posts fields = select fields "posts"
--     first <- run $ findOne (posts [])
--     print first
-- 
-- 
-- 
-- -- note that it doesn't even make any sense to have ANY of these fields missing. They'll need defaults if anything
-- -- "" for all strings, and [] for tags
-- -- NOTE: This fails if the fields are missing. Not sure how to handle that :)
-- -- I'm ok with default values, but I don't even know really how to detect them
-- data Post = EmptyPost | Post {author :: String, text :: String, title :: String, tags :: [String]} deriving (Show)
-- parsePost :: Document -> Post
-- parsePost doc = Post { author = get "author"
--                      , text = get "text"
--                      , title = get "title"
--                      , tags = get "tags"
--                      }
--     where get f = typed $ value f
--           value f = valueAt f doc
-- 
-- parseExamples = do
--     -- run simpleExamples first
--     pipe <- runIOE $ connect $ host "127.0.0.1"
--     let run act = access pipe master "tesths" act   
--     Right (Just doc) <- run $ findOne $ select ["author" =: "Eliot"] "posts"
--     print doc
--     print $ valueAt "title" doc 
--     print $ parsePost doc -- NOTE: This fails because it can't find title. How the crap do I do this?
--     print "getting closer"
-- 
