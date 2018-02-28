module ApiServer.Middleware (middleware) where
import Network.Wai (Application)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger

middleware :: Application -> Application
middleware = logStdoutDev . staticPolicy (addBase "static")
