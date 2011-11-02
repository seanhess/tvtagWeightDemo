{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Control.Monad.Reader
import Hack2.Contrib.Response
import Hack2.Handler.SnapServer
import Hack2.Contrib.Utils (show_bytestring)

import Network.Miku
import Network.Miku.Engine
import Network.Miku.Utils
import Network.Miku.Type
import Data.Maybe
import Air.Env hiding ((.))
import Prelude ((.))

import Hack2.Contrib.Request
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as Lazy

import Hack2.Contrib.Middleware.SimpleAccessLogger

import Text.JSON
import Text.JSON.Generic

-- default on port 3000

-- some helper methods for json
sendJson :: B.ByteString -> AppMonad
sendJson x = send "application/json; charset=utf-8; charset=utf-8" x

json :: (Data a) => a -> AppMonad
json x = sendJson $ B.pack $ encode $ toJSON x

-- I'm not sure something like this exists
send :: String -> B.ByteString -> AppMonad
send mimeType x = do
    update - set_content_type (B.pack mimeType)
    update - set_body_bytestring - Lazy.fromChunks [x]

data Thang = Thang { ttt :: String } deriving (Eq, Show, Typeable, Data)

main :: IO ()
main = do
    putStrLn "server started on port 3000..."
  
    run . miku - do
    
        -- before return
        -- after return

        middleware - simple_access_logger Nothing

        -- params are ? params
        get "/bench" - do
          name <- ask ^ params ^ lookup "name" ^ fromMaybe "nobody"
          html ("<h1>" + name + "</h1>")

        -- simple
        get "/hello"    (text "hello world")

        get "/json" - do
            json (Thang "wooot")

        get "/bson" - do
            text "HI"

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
        get "/html"     (html "<html><body><p>miku power!</p></body></html>")
        
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
