{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module ApiServer
    ( apiServer
    ) where

import Servant
import Network.Wai.Handler.Warp (run)
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Exception (try)
import Control.Monad.Except (liftIO)
import Data.Text.Lazy (Text)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import ApiServer.Middleware (middleware)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON(..), toJSON, fromJSON, Value(..), encode)

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


data Info = Info
  { infoContent :: String
  } deriving (Show)

data SymbolArchive = SymbolArchive
  { symbolContent :: String
  } deriving (Show, Generic)

instance ToJSON Info where
    toJSON  (Info c) = toJSON c

instance FromJSON SymbolArchive

type EntrypointResource = Get '[HTML] Text
type InfoResource       = "info" :> Get '[JSON] Info
type SymbolsResource    = "symbols" :> ReqBody '[JSON] SymbolArchive :> Post '[JSON] NoContent

type ServerAPI =
        EntrypointResource
        :<|>   InfoResource
        :<|>   SymbolsResource


serverRoutes :: Server ServerAPI
serverRoutes =
         entrypointHandler
    :<|> infoHandler
    :<|> symbolsHandler
  where
    entrypointHandler :: Handler Text
    entrypointHandler = return "Welcome to my awesome Haskell-API!" :: Handler Text

    infoHandler :: Handler Info
    infoHandler = return $ Info "Important status information about the server"

    symbolsHandler :: SymbolArchive -> Handler NoContent
    symbolsHandler syms = do
      liftIO $ putStrLn ("Test symbols: " ++ symbolContent syms)
      return NoContent

serverProxy :: Proxy ServerAPI
serverProxy = Proxy

router :: Application
router = serve serverProxy serverRoutes

apiServer :: IO ()
apiServer = run 8088 (middleware router)
