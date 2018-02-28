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
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import ApiServer.Middleware (middleware)

data HTML

instance MimeRender HTML Text where
  mimeRender _ = encodeUtf8

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

type ServerAPI =
        Get '[HTML] Text
  :<|>  "symbols" :> Post '[HTML] Text


serverRoutes :: Server ServerAPI
serverRoutes = return "Hello from my API!" :<|> return "Hello from upload"

serverProxy :: Proxy ServerAPI
serverProxy = Proxy


router :: Application
router = serve serverProxy serverRoutes

apiServer :: IO ()
apiServer = run 80 (middleware router)
