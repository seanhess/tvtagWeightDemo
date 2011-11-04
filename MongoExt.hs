{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, TypeSynonymInstances, IncoherentInstances #-}

{-

MDB is a function to pass around

    db <- mdb 
    db $ delete $ select [] "tags"

-}

module MongoExt (mdb) where

import Data.Aeson
import qualified Data.Aeson.Types as T

import Numeric

import Data.Attoparsec (parse, Result(..))
import Data.Attoparsec.Number (Number(..))
import qualified Data.Text as Text
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as BS
-- Aeson's "encode" to JSON generates lazy bytestrings
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.CompactString as CS

import Database.MongoDB
import Data.Bson
import qualified Data.Bson as Bson
import qualified Data.Vector

import qualified Data.CompactString as CS
import Control.Monad.IO.Class

import GHC.Int


-- CONNECTION METHODS --
mdb :: String -> String -> IO (Action IO a -> IO (Either Failure a))
mdb hostname dbname = do
    pipe <- runIOE $ connect $ host hostname
    return (access pipe master (CS.pack dbname))






-- MONGDB JSON SERIALIZATION --

csToTxt :: UString -> Text.Text
csToTxt cs = Text.pack $ CS.unpack cs

bsToTxt :: BS.ByteString -> Text.Text
bsToTxt bs = Text.pack $ BS.unpack bs

fieldToPair :: Field -> T.Pair
fieldToPair f = key .= val
        where key = csToTxt $ label f
              val = toJSON (value f)

instance ToJSON Document where
    toJSON fs = object $ map fieldToPair fs
          
instance ToJSON Data.Bson.Value where
    toJSON (Float f) = T.Number $ D f
    toJSON (Bson.String s) = T.String $ csToTxt s
    toJSON (Bson.Array xs) = T.Array $ Data.Vector.fromList (map toJSON xs)
    toJSON (Doc fs) = object $ map fieldToPair fs
    toJSON (Uuid (UUID bs)) = T.String $ bsToTxt bs
    toJSON (Bson.Bool b) = T.Bool b
    toJSON (Int32 i) = T.Number (I (fromIntegral i))
    toJSON (Int64 i) = T.Number (I (fromIntegral i))

    toJSON (ObjId (Oid w32 w64)) = T.String $ Text.pack $ showHex w32 "" ++ showHex w64 ""
    toJSON (UTC time) = T.String "look up Data.Time.Clock.UTC.UTCTime"

    toJSON (Md5 m) = T.Null
    toJSON (UserDef u) = T.Null
    toJSON (Bin b) = T.Null
    toJSON (Fun f) = T.Null
    toJSON Bson.Null = T.Null
    toJSON (RegEx r) = T.Null
    toJSON (JavaScr j) = T.Null
    toJSON (Sym s) = T.Null
    toJSON (Stamp s) = T.Null
    toJSON (MinMax mm) = T.Null



















-- TESTS -- 

mdbExample :: IO ()
mdbExample = do
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
