{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module ApiServer
    ( apiServer
    ) where

import Servant.API ((:<|>) ((:<|>)), (:>), BasicAuth, Get, JSON)

import Api (AppAPI, Info(..), User(..), SymbolArchive(..))
import Servant (Proxy, BasicAuthData(..), NoContent(..), Proxy(..))
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

serverRoutes :: Server AppAPI
serverRoutes =
         entrypointHandler
    :<|> infoHandler
    :<|> symbolsHandler
  where
    entrypointHandler :: Handler Text
    entrypointHandler = return "Welcome to my awesome Haskell-API!" :: Handler Text

    infoHandler :: Handler Info
    infoHandler = return $ Info "Important status information about the server"

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

serverProxy :: Proxy AppAPI
serverProxy = Proxy

router :: Application
router = serveWithContext serverProxy authContext serverRoutes

apiServer :: IO ()
apiServer = run 8088 (middleware router)
