{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad.Reader
import Hack2.Contrib.Response
import Hack2.Handler.SnapServer
import Hack2.Contrib.Utils (show_bytestring)

import Data.Aeson (ToJSON(..), encode, (.=), object)
import qualified Data.Aeson

import Network.Miku hiding (html)
import qualified Network.Miku
import Network.Miku.Engine
import Network.Miku.Utils
import Network.Miku.Type
import Data.Maybe
import Air.Env hiding ((.), object, div, head, (/))
import Prelude ((.))

import Hack2.Contrib.Request hiding (host)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as Lazy

import Hack2.Contrib.Middleware.SimpleAccessLogger

import Database.MongoDB.Connection (host)
import Database.MongoDB (Failure, Value(..), Document, runIOE, access, master, connect, Host)
import Data.Bson ((=:), valueAt)
import qualified Data.CompactString as CS
import qualified Data.Bson

import Tags
import qualified Tags
import MongoExt

import Text.Hastache
import Text.Hastache.Context

import Control.Monad.IO.Class

-- some helper methods for json
sendJson :: B.ByteString -> AppMonad
sendJson x = send "application/json; charset=utf-8; charset=utf-8" x

json :: (ToJSON a) => a -> AppMonad
json x = sendJson $ l2b $ encode x

l2b :: Lazy.ByteString -> B.ByteString
l2b ls = B.concat $ Lazy.toChunks ls





-- I'm not sure something like this exists
send :: String -> B.ByteString -> AppMonad
send mimeType x = do
    update - set_content_type (B.pack mimeType)
    update - set_body_bytestring - Lazy.fromChunks [x]

sendHtml = Network.Miku.html

instance ToJSON Failure where
    toJSON f = Data.Aeson.Null

-- Sample Data Object for JSON --
data Thang = Thang { ttt :: String } deriving (Eq, Show)
instance ToJSON Thang where
    toJSON (Thang ttt) = object ["ttt" .= ttt]




-- Converting Documents to HTML Templating --
valueToMu :: Value -> MuType m
valueToMu (Data.Bson.String v) = MuVariable $ Data.Bson.unpack v
valueToMu v = MuVariable $ show v 

docContext :: (Monad m) => Document -> MuContext m
docContext source = mkStrContext ctx
    where ctx name = valueToMu (Data.Bson.valueAt (CS.pack name) source) 


plusToSpace c = if (c == '+') then ' ' else c 


tagsContext tags = MuList $ map tagContext tags 
    where tagContext tag = mkStrContext tagCtx
            where tagCtx "title" = MuVariable $ Tags.title tag
                  tagCtx "url"   = MuVariable $ Tags.url tag
                  tagCtx "source" = MuVariable $ Tags.source tag
                  tagCtx "score" = MuVariable $ Tags.score tag
                  tagCtx "term" = MuVariable $ Tags.term tag 

-- renderTagsJson result = do
--     let Right tags = result
--     json tags


main :: IO ()
main = do
    putStrLn "server started on port 3000..."

    pipe <- connectTagsDb
    runTags pipe populateMockData

    -- views --
    tagsView <- readFile "views/tags.mustache"
  
    -- run the server --
    run . miku - do
    
        -- before return
        -- after return

        middleware - simple_access_logger Nothing

        get "/tags.json" - do
            Right result <- liftIO $ runTags pipe $ rawFind "Lady Gaga"  -- without liftIO, I die
            json result
        
        get "/tags.html" - do
            result <- liftIO $ runTags pipe $ rawFind "Lady Gaga"
            let Right tags = result
            
            let context "tags" = MuList $ map docContext tags 
        
            res <- hastacheStr defaultConfig (encodeStr tagsView) (mkStrContext context) 
            sendHtml $ l2b res

        get "/manual/:term" - do
            caps <- captures
            let term = (lookup "term" ^ fromMaybe "") caps
            let cleanTerm = B.map plusToSpace term
            Right tags <- liftIO $ runTags pipe $ findManual $ bsToCs cleanTerm
            
            let context "tags" = tagsContext tags
        
            res <- hastacheStr defaultConfig (encodeStr tagsView) (mkStrContext context) 
            sendHtml $ l2b res

        get "/sourceWeighted/:term" - do
            caps <- captures
            let term = (lookup "term" ^ fromMaybe "") caps
            let cleanTerm = B.map plusToSpace term
            Right tags <- liftIO $ runTags pipe $ findSourceWeighted $ bsToCs cleanTerm
            let context "tags" = tagsContext tags
            res <- hastacheStr defaultConfig (encodeStr tagsView) (mkStrContext context) 
            sendHtml $ l2b res
            

        get "/termScored/:term" - do
            caps <- captures
            let term = (lookup "term" ^ fromMaybe "") caps
            let cleanTerm = B.map plusToSpace term
            Right tags <- liftIO $ runTags pipe $ lazyFindTermScored $ B.unpack cleanTerm
            let context "tags" = tagsContext tags
            res <- hastacheStr defaultConfig (encodeStr tagsView) (mkStrContext context) 
            sendHtml $ l2b res        


        -- params are ? params
        get "/bench" - do
          name <- ask ^ params ^ lookup "name" ^ fromMaybe "nobody"
          sendHtml ("<h1>" + name + "</h1>")

        -- simple
        get "/hello"    (text "hello world")

        get "/json" - do
            json (Thang "wooot")

        get "/bson" - do
            json ["key" =: "value", "woot" =: "henry", "sub" =: ["one" =: 1], "arr" =: [1,2,3,4]]

        get "/hello/:name" - do
            caps <- captures
            text ("HI " + (lookup "name" ^ fromMaybe "nobody") caps)
            -- text . show_bytestring =<< captures
  
        get "/debug"                  (text . show_bytestring =<< ask)
        get "/debug/:param/:again" - do
            env <- ask 
            (text . show_bytestring) env
          
        -- io
        get "/source"    - text =<< io (B.readFile "api.hs")
        
        -- html output
        get "/html"     (sendHtml "<html><body><p>miku power!</p></body></html>")

        get "/slow" - do
            liftIO $ sleep 5
            text "Done"
        
        get "/" - do
          update - set_status 203
          text "match /"
        
        get "/test-star/*/hi" - do
          text "test-star/*/hi"
        
        -- public serve, only allows /src
        public (Just "./") ["/public"]

        -- default
        get "*" - do
          text "match everything"
        
          
        -- -- treat .hs extension as text/plain
        -- mime "hs" "text/plain"
