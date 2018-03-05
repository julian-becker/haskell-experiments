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

import Servant.API ((:<|>) ((:<|>)), (:>), OctetStream, BasicAuth, Get, JSON, NoContent, Post, Get, ReqBody)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON(..), toJSON, fromJSON, Value(..), encode)


data InfoRes = InfoRes
  { infoContent :: String
  } deriving (Show, Generic)

instance ToJSON InfoRes
instance FromJSON InfoRes


data SymbolArchive = SymbolArchive
  { symbolContent :: ByteString
  } deriving (Show, Generic)


data User = User
  { userName :: String
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User


type EntrypointResource = Get '[JSON] Text
type InfoResource       = "info" :> Get '[JSON] InfoRes
type SymbolsResource    = "symbols" :> BasicAuth "symbols" User :> ReqBody '[OctetStream] SymbolArchive :> Post '[JSON] NoContent

type AppAPI =
        EntrypointResource
        :<|>   InfoResource
        :<|>   SymbolsResource
