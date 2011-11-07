{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad.Reader
import Hack2.Contrib.Response
import Hack2.Handler.SnapServer
import Hack2.Contrib.Utils (show_bytestring)

import Data.Aeson (ToJSON(..), encode, (.=), object)
import qualified Data.Aeson

import Network.Miku 
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

import Control.Monad.IO.Class





-- I'm not sure something like this exists
send :: String -> B.ByteString -> AppMonad
send mimeType x = do
    update - set_content_type (B.pack mimeType)
    update - set_body_bytestring - Lazy.fromChunks [x]

-- Sample Data Object for JSON --
data Thang = Thang { ttt :: String } deriving (Eq, Show)
instance ToJSON Thang where
    toJSON (Thang ttt) = object ["ttt" .= ttt]



sendJson :: B.ByteString -> AppMonad
sendJson x = send "application/json; charset=utf-8; charset=utf-8" x

json :: (ToJSON a) => a -> AppMonad
json x = sendJson $ l2b $ encode x

l2b :: Lazy.ByteString -> B.ByteString
l2b ls = B.concat $ Lazy.toChunks ls



main :: IO ()
main = do
    putStrLn "server started on port 3000..."

    -- run the server --
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
