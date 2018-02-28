{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module ApiServer
    ( apiServer
    ) where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Text.Lazy (Text)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import ApiServer.Middleware (middleware)
import Data.Aeson (ToJSON, toJSON, Value(..), encode)

data HTML
data JsonDoc

instance MimeRender HTML Text where
  mimeRender _ = encodeUtf8

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")



instance MimeRender JsonDoc Value where
  mimeRender _ = encode

instance Accept JsonDoc where
  contentType _ = "application" // "json" /: ("charset", "utf-8")


data MyData = MyData
  { content :: String
  } deriving (Show)


instance ToJSON MyData where
    toJSON  (MyData c) = toJSON c

type ServerAPI =
        Get '[HTML] Text
  :<|>  "symbols" :> Post '[JSON] MyData

symbolsHandler :: MyData
symbolsHandler = MyData "Hello from upload"

serverRoutes :: Server ServerAPI
serverRoutes = return "Hello from my API!" :<|> return symbolsHandler

serverProxy :: Proxy ServerAPI
serverProxy = Proxy


router :: Application
router = serve serverProxy serverRoutes

apiServer :: IO ()
apiServer = run 80 (middleware router)
