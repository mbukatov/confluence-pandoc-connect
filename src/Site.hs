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
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                            as A
import qualified Data.Aeson.Lens                       as A
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.CaseInsensitive                  as CI
import           Data.Foldable                         (traverse_)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as E
import           Data.Text.Lens                        (unpacked)
import           Data.Version                          (showVersion)
import           Heist
import qualified Heist.Interpreted                     as I
import           Key
import           LifecycleHandlers
import           Network.HTTP.Client                   (RequestBody (..),
                                                        requestBody)
import           Network.HTTP.Client.MultipartFormData as MFD
import           Network.HTTP.Types.Header
import           Page
import           Paths_confluence_pandoc_connect       (version)
import           Prelude
import           Snap.AtlassianConnect
import qualified Snap.AtlassianConnect.HostRequest     as HR
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileUploads
import           System.Environment                    (getEnv)
import           TenantJWT
import           Text.Pandoc
import           Text.Pandoc.MediaBag
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

