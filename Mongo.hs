{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

{-
    pipe <- mconnect "127.0.0.1"
    let db = mdb pipe
    db $ delete $ select [] "tags"
-}

module Mongo (mdb, mconnect) where

import Database.MongoDB
import Data.Bson
import Data.CompactString ()
import Control.Monad.IO.Class

type Hostname = String

mdb :: (MonadIO m) => Pipe -> Action m a -> m (Either Failure a)
mdb pipe act = access pipe master "testtags" act

mconnect :: Hostname -> IO Pipe
mconnect hostIp = runIOE $ connect $ host hostIp
