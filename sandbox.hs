{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Mongo
import Database.MongoDB
import Tags
import MockTags

-- data TagInfo a = EmptyInfo | TagInfo a deriving (Show, Read)
-- data Point = Point Float Float deriving (Show) 
-- data SearchTag = SearchTag { keyword :: String 
--                            , weight :: Int
--                            , info :: TagInfo Int
--                            } deriving (Show)
-- 
-- 
-- 
-- woot = SearchTag "henry" 10 EmptyInfo

-- Add the Tags to test
testItOut = do
    pipe <- mconnect "127.0.0.1"
    let db = mdb pipe

    -- remove everything
    db $ delete $ select [] "tags"

    insertTags db mockGaga
    print "DONE"

    Right tags <- findTags db "Lady Gaga"
    print tags


    Right regtags <- findTags db "Revenge"
    print $ length regtags
    print $ regtags
    -- db $ insertTags mockGaga
    -- db $ insert "tags" post

main = testItOut
    

