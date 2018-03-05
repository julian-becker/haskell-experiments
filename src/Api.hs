{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api
    ( AppAPI
    , InfoRes(..)
    , SymbolArchive(..)
    , User(..)
    ) where

import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, Get, JSON, NoContent, Post, Get, ReqBody)

import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON(..), toJSON, fromJSON, Value(..), encode)


data InfoRes = InfoRes
  { infoContent :: String
  } deriving (Show, Generic)

instance ToJSON InfoRes
instance FromJSON InfoRes


data SymbolArchive = SymbolArchive
  { symbolContent :: String
  } deriving (Show, Generic)

instance FromJSON SymbolArchive


data User = User
  { userName :: String
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User


type EntrypointResource = Get '[JSON] Text
type InfoResource       = "info" :> Get '[JSON] InfoRes
type SymbolsResource    = "symbols" :> BasicAuth "symbols" User :> ReqBody '[JSON] SymbolArchive :> Post '[JSON] NoContent

type AppAPI =
        EntrypointResource
        :<|>   InfoResource
        :<|>   SymbolsResource
