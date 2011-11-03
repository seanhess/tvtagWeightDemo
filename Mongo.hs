{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

{-
    This lets you pass "db" around (a function accepting an Action, and returning an IO Either Failure)
    https://github.com/TonyGen/mongoDB-haskell/blob/master/doc/tutorial.md

    But I'm going to try this:
    https://github.com/TonyGen/mongoDB-haskell/blob/master/doc/Example.hs
    
    db <- mdb 
    db $ delete $ select [] "tags"
-}

module Mongo (mdb) where

import Database.MongoDB
import Data.Bson
import Data.CompactString
import Control.Monad.IO.Class

main :: IO ()
main = do
    -- Here's the normal way you access mongo stuff
    -- I want to put it all in one function, and return a function called "db"
    -- so I can think of it more like normal mongo: db.mycollection.remove({})
    pipe <- runIOE $ connect $ host "127.0.0.1"
    let run action = access pipe master "testdb" action
    run $ delete $ select [] "mycollection"
        
    -- here's what the shortcut looks like
    db <- mdb "127.0.0.1" "testdb" 
    -- db $ delete $ select [] "mycollection"
    -- but without the above line, I get the error pasted at the bottom. Why?

    return () 


-- The type
mdb :: String -> String -> IO (Action IO a -> IO (Either Failure a))
mdb hostname dbname = do
    pipe <- runIOE $ connect $ host hostname
    return (access pipe master (pack dbname))

