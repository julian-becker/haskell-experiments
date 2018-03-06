{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApiServer
    ( apiServer
    ) where

import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, Get, JSON)
import ApiDocs (apiDocs, DocumentedAPI)
import Api (AppAPI, InfoRes(..), User(..), SymbolArchive(..))
import Servant (Proxy, AuthProtect(..), BasicAuthData(..), NoContent(..), Proxy(..), Accept(..), MimeUnrender(..))
import Servant.Server (serveWithContext, Application, BasicAuthCheck(..), BasicAuthResult(..), Context(..), Handler, Server)
import Servant.API (OctetStream)
import Network.Wai.Handler.Warp (run)
import Servant.Swagger.UI
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Exception (try)
import Control.Monad.Except (liftIO)
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import ApiServer.Middleware (middleware)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON(..), toJSON, fromJSON, Value(..), encode)
import Control.Lens ((.~), (?~), (%~), (&), mapped)


instance MimeUnrender OctetStream SymbolArchive where
  mimeUnrender _ = Right . SymbolArchive

serverRoutes :: Server DocumentedAPI
serverRoutes =
         (entrypointHandler
    :<|> infoHandler
    :<|> symbolsHandler)
    :<|> swaggerSchemaUIServer apiDocs
  where
    entrypointHandler :: Handler Text
    entrypointHandler = return "Welcome to my awesome Haskell-API!" :: Handler Text

    infoHandler :: Handler InfoRes
    infoHandler = return $ InfoRes "Important status information about the server"

    symbolsHandler :: User -> SymbolArchive -> Handler NoContent
    symbolsHandler u syms = do
      liftIO $ L8.writeFile "outfile" $ symbolContent syms
      return NoContent

authCheck :: BasicAuthCheck User
authCheck = let
      check (BasicAuthData user pass) =
        if user == "admin"
          then return (Authorized $ User "Administrator")
          else return Unauthorized
    in BasicAuthCheck check

authContext :: Context (BasicAuthCheck User ': '[])
authContext = authCheck :. EmptyContext

serverProxy :: Proxy DocumentedAPI
serverProxy = Proxy

router :: Application
router = serveWithContext serverProxy authContext serverRoutes

apiServer :: IO ()
apiServer = run 8088 (middleware router)
