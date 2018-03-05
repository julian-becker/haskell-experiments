{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# language FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ApiDocs
    ( apiDocs
    , DocumentedAPI
    ) where

import Servant.API ((:<|>),(:>), BasicAuth, Get, JSON)
import Api (AppAPI, InfoRes(..), User(..), SymbolArchive(..))
import Servant (Proxy(..), NoContent(..), MimeUnrender(..))
import Data.Text.Lazy (Text)
import Data.Aeson (ToJSON, toJSON)
import Servant.Swagger (toSwagger,HasSwagger)
import Data.Swagger (Swagger, URL(..), ToSchema(..), ToParamSchema(..), NamedSchema(..),
  schema, example, description, binarySchema, defaultSchemaOptions, genericDeclareNamedSchema)
import Control.Lens ((?~), (&), mapped)

type SwaggerAPI    = "swagger.json" :> Get '[JSON] Swagger
type DocumentedAPI = AppAPI :<|> SwaggerAPI

apiDocs :: Swagger
apiDocs = toSwagger (Proxy :: Proxy AppAPI)

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

instance ToSchema SymbolArchive where
  declareNamedSchema _ = return $ NamedSchema (Just "SymbolArchive") binarySchema
