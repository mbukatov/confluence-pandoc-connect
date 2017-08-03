{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module LifecycleHandlers
    ( lifecycleRoutes
    , validHostName
    ) where

import           Application
import qualified Control.Arrow              as ARO
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.List                  (find)
import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import qualified Network.URI                as NU
import           Persistence.PostgreSQL
import qualified Persistence.Tenant         as PT
import qualified Snap.AtlassianConnect      as AC
import qualified Snap.Core                  as SC
import qualified Snap.Snaplet               as SS
import qualified SnapHelpers                as SH
import           TenantJWT                  (withMaybeTenant)

lifecycleRoutes :: [(BC.ByteString, SS.Handler App App ())]
lifecycleRoutes = fmap (ARO.first BC.pack) standardHandlers

standardHandlers :: [(String, SS.Handler App App ())]
standardHandlers =
  [ ("/installed"          , installedHandler)
  , ("/uninstalled"        , uninstalledHandler)
  ]

installedHandler :: SS.Handler b App ()
installedHandler = do
  lr <- AC.getLifecycleResponse
  maybe lifecycleResponseErrorResponse installedHandlerWithTenant lr

installedHandlerWithTenant :: AC.LifecycleResponse -> SS.Handler b App ()
installedHandlerWithTenant tenantInfo = do
   validHosts <- fmap AC.connectHostWhitelist AC.getConnect
   if validHostName validHosts tenantInfo
      then insertTenantInfo tenantInfo >>= maybe tenantInsertionFailedResponse (const SH.respondNoContent)
      else domainNotSupportedResponse
   where
      tenantInsertionFailedResponse = SH.respondWithError SH.unauthorised "Failed to insert the new tenant. Not a valid host or the tenant information was invalid."
      domainNotSupportedResponse = SH.respondWithError SH.unauthorised $ "Your domain is not supported by this addon. Please contact the developers. " ++ (show . tenantAuthority $ tenantInfo)

insertTenantInfo :: AC.LifecycleResponse -> SS.Handler b App (Maybe Integer)
insertTenantInfo info = withMaybeTenant $ \mt -> SS.with db $ withConnection (\conn -> PT.insertTenantInformation conn (fst <$> mt) info)

validHostName:: [AC.HostName] -> AC.LifecycleResponse -> Bool
validHostName validHosts tenantInfo = isJust maybeValidhost
   where
      authorityMatchHost auth host = T.toLower host `T.isSuffixOf` (T.toLower . T.pack . NU.uriRegName $ auth)
      maybeValidhost = do
         auth <- tenantAuthority tenantInfo
         find (authorityMatchHost auth) validHosts

tenantAuthority :: AC.LifecycleResponse -> Maybe NU.URIAuth
tenantAuthority = NU.uriAuthority . AC.getURI . AC.lrBaseUrl

uninstalledHandler :: SS.Handler b App ()
uninstalledHandler = do
   mTenantInfo <- AC.getLifecycleResponse
   maybe lifecycleResponseErrorResponse uninstalledHandlerWithTenant mTenantInfo

uninstalledHandlerWithTenant :: AC.LifecycleResponse -> SS.Handler b App ()
uninstalledHandlerWithTenant tenantInfo = do
   potentialTenant <- withConnection $ \conn -> PT.lookupTenant conn (AC.lrClientKey tenantInfo)
   case potentialTenant of
      Nothing -> SH.respondWithError SH.notFound "Tried to uninstall a tenant that did not even exist."
      Just tenant -> withConnection (PT.hibernateTenant tenant) >> SH.respondNoContent

lifecycleResponseErrorResponse :: SS.Handler b App ()
lifecycleResponseErrorResponse = do
  badBody <- SC.readRequestBody (1024 * 10)
  SC.logError $ "Failed to parse lifecycle response: " `BC.append` BLC.toStrict badBody
  SH.respondBadRequest
