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

import Api (AppAPI, InfoRes(..), User(..), SymbolArchive(..))
import Servant (Proxy, AuthProtect(..), BasicAuthData(..), NoContent(..), Proxy(..))
import Servant.Server (serveWithContext, Application, BasicAuthCheck(..), BasicAuthResult(..), Context(..), Handler, Server)
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
import Servant.Swagger (toSwagger,HasSwagger)
import Data.Swagger (Swagger, URL(..), ToSchema(..),
  parameters, allOperations, Param(..), Referenced(..), ToParamSchema, ParamAnySchema(..),
  security, required, name, toParamSchema, paramSchema, ParamLocation(..),
  info, title, description, license, url, version, genericDeclareNamedSchema, schema, example,
  defaultSchemaOptions, in_)
import Control.Lens ((.~), (?~), (%~), (&), mapped)

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type API = AppAPI :<|> SwaggerAPI

api :: Proxy AppAPI
api = Proxy

addParam :: Param -> Swagger -> Swagger
addParam param = allOperations.parameters %~ (Inline param :)

-- instance (HasSwagger sub, ToParamSchema a)
--   => HasSwagger (BasicAuth realm a :> sub) where
--   toSwagger _ = toSwagger (Proxy :: Proxy sub)
--       & addParam basicAuthHeader
--       & security .~ requirements
--     where
--       requirements = []
--       basicAuthHeader = mempty
--         & required .~ Just True
--         & name .~ "Authorization"
--         & description .~ Just "Basic base64(name:password)"
--         & schema .~ ParamOther (mempty
--              & in_ .~ ParamHeader
--              & paramSchema .~ toParamSchema (Proxy :: Proxy a))


instance (HasSwagger sub, ToParamSchema a)
  => HasSwagger (BasicAuth realm a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)



instance ToSchema InfoRes where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some Info field"
    & mapped.schema.example ?~ toJSON (InfoRes "some text")

instance ToSchema Swagger where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema NoContent
instance ToSchema User
instance ToParamSchema User
instance ToSchema SymbolArchive

apiSwagger :: Swagger
apiSwagger = toSwagger api

serverRoutes :: Server API
serverRoutes =
         (entrypointHandler
    :<|> infoHandler
    :<|> symbolsHandler)
    :<|> (return apiSwagger)
  where
    entrypointHandler :: Handler Text
    entrypointHandler = return "Welcome to my awesome Haskell-API!" :: Handler Text

    infoHandler :: Handler InfoRes
    infoHandler = return $ InfoRes "Important status information about the server"

    symbolsHandler :: User -> SymbolArchive -> Handler NoContent
    symbolsHandler u syms = do
      liftIO $ putStrLn ("Test symbols: " ++ symbolContent syms)
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

serverProxy :: Proxy API
serverProxy = Proxy

router :: Application
router = serveWithContext serverProxy authContext serverRoutes

apiServer :: IO ()
apiServer = run 8088 (middleware router)
