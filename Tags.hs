{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Tags (findTags, insertTags) where

import MockTags
import Data.Bson
import Control.Applicative
import Control.Monad.IO.Class
import Database.MongoDB

-- Better yet, can I make this function return type: Action m a -> m (Either Failure a)
-- I need to know more first
insertTags :: (MonadIO m1, Applicative m1, Monad m) => (Action m1 Value -> m b) -> [Document] -> m ()
insertTags db tags = mapM_ (db.(insert "tags")) tags

-- findTags db term = db $ find (select ["title" =: term] "tags") >>= rest
-- findTagsGreater db term = db $ find (select ["title" =: ["$gte" =: term]] "tags") >>= rest
findTags db term = db $ find (select ["title" =: Regex term "i"] "tags") >>= rest



















































