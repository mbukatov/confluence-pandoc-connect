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
import           Healthcheck
import           LifecycleHandlers
import qualified MicrosZone                      as MZ
import           MigrationHandler
import           Prelude
import           Snap.AtlassianConnect
import           Snap.Core
import qualified Snap.Logging.Json               as SLJ
import           Snap.Logging.Json.AccessLogging
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           System.Environment              (getEnv)
import           System.IO                       (stdout)
import           WithToken

handleCreateRequest :: AppHandler ()
handleCreateRequest =
  method GET (withTokenAndTenant renderFileForm) <|>
  method POST convertFileFromFormData

-- | The application's routes.
routes, applicationRoutes :: [(BS.ByteString, AppHandler ())]
routes = map (\(b, a) -> (b, a >> with logging logAccess)) $ applicationRoutes ++ lifecycleRoutes
applicationRoutes =
  [ ("/rest/heartbeat", heartbeatRequest)
  , ("/rest/migration", migrationRequest)
  , ("/version.json",   versionJson)
  , ("/create",         handleCreateRequest)
  ]

dbConfigFromEnv :: IO PGSConfig
dbConfigFromEnv = pgsDefaultConfig . BC.pack <$> getEnv "PG_CONFLUENCE_PANDOC_CONNECT_URL"

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "confluence-pandoc-connect" "CPC connect application" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    dbConfig <- liftIO dbConfigFromEnv
    db' <- nestSnaplet "db" db $ pgsInit' dbConfig
    zone <- liftIO MZ.fromEnv
    let
      connectFeatures = ConnectFeatures { webItemDisplayEnabled = zone == Just MZ.Dev || isNothing zone }
      descriptorWithZone = MZ.modifyDescriptorUsingZone zone $ addonDescriptor connectFeatures
    ac <- nestSnaplet "connect" connect $ initConnectSnaplet descriptorWithZone
    logger <- nestSnaplet "logging" logging $ SLJ.initSnaplet stdout
    addRoutes routes
    return $ App h db' ac logger

