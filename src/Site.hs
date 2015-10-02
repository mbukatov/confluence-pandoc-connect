{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

import           Application
import           AtlassianConnect
import           Confluence
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as BC
import           Data.Maybe
import qualified Data.Text                       as T
import           Data.Version                    (showVersion)
import           LifecycleHandlers
import           Paths_confluence_pandoc_connect (version)
import           Prelude
import           Snap.AtlassianConnect
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           System.Environment              (getEnv)
import           WithToken

heartbeatRequest :: AppHandler ()
heartbeatRequest = do
  putResponse $ setResponseCode 200 emptyResponse
  writeText . T.pack $ showVersion version

handleCreateRequest :: AppHandler ()
handleCreateRequest =
  method GET (withTokenAndTenant renderFileForm) <|>
  method POST convertFileFromFormData

-- | The application's routes.
routes, applicationRoutes :: [(BS.ByteString, AppHandler ())]
routes = applicationRoutes ++ lifecycleRoutes
applicationRoutes =
  [ ("rest/heartbeat", heartbeatRequest)
  , ("/create", handleCreateRequest)
  ]

dbConfigFromEnv :: IO PGSConfig
dbConfigFromEnv = pgsDefaultConfig . BC.pack <$> getEnv "PG_CONFLUENCE_PANDOC_CONNECT_URL"

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "confluence-pandoc-connect" "CPC connect application" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    dbConfig <- liftIO dbConfigFromEnv
    db' <- nestSnaplet "db" db $ pgsInit' dbConfig
    ac <- nestSnaplet "connect" connect $ initConnectSnaplet addonDescriptor
    addRoutes routes
    return $ App h db' ac

