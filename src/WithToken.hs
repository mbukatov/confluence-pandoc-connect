{-# LANGUAGE OverloadedStrings #-}

module WithToken where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BSC
import qualified Data.CaseInsensitive   as DC
import qualified Data.Time.Clock        as DTC
import qualified Persistence.PostgreSQL as PP
import qualified Persistence.Tenant     as TN
import qualified Snap.AtlassianConnect  as AC
import qualified Snap.Core              as SC
import qualified SnapHelpers            as SH
import           TenantJWT              (withTenant)

import           Application

acHeaderName :: DC.CI BSC.ByteString
acHeaderName = DC.mk . BSC.pack $ "X-acpt" -- See atlassian-connect-play-java PageTokenValidatorAction#TOKEN_KEY

tenantFromToken :: (AC.TenantWithUser -> AppHandler a) -> AppHandler (Maybe a)
tenantFromToken tenantApply = do
  --request <- SC.getRequest
  maybeToken <- SC.getPostParam "page-token"
  --let potentialTokens = SC.getHeaders acHeaderName request
  case maybeToken of
    Nothing -> SH.respondWithError SH.badRequest "You need to provide a page token in the headers to use this resource. None was provided." >> return Nothing
    Just acTokenHeader -> do
      connectData <- AC.getConnect
      let potentiallyDecodedToken = AC.decryptPageToken (AC.connectAES connectData) acTokenHeader
      case potentiallyDecodedToken of
         Left errorMessage -> do
           SH.respondWithError SH.badRequest $ "Error decoding the token you provided: " ++ errorMessage
           return Nothing
         Right pageToken -> do
            let tokenExpiryTime = DTC.addUTCTime (fromIntegral . AC.connectPageTokenTimeout $ connectData) (AC.pageTokenTimestamp pageToken)
            currentTime <- liftIO DTC.getCurrentTime
            if DTC.diffUTCTime currentTime tokenExpiryTime < 0
               then do
                  -- TODO the token has not expired, so we need to load the tenant and give it
                  -- back to the program
                  potentialTenant <- lookupTenantWithPageToken pageToken
                  case potentialTenant of
                     Nothing -> do
                       SH.respondWithError SH.notFound "Your page token was valid but the tenant could not be found. Maybe it no longer exists."
                       return Nothing
                     Just tenant -> Just <$> tenantApply tenant
               else do
                 SH.respondWithError SH.unauthorised "Your token has expired. Please refresh the page."
                 return Nothing

lookupTenantWithPageToken :: AC.PageToken -> AppHandler (Maybe AC.TenantWithUser)
lookupTenantWithPageToken pageToken =
  PP.withConnection $ \conn ->
    fmap (flip (,) (AC.pageTokenUser pageToken)) <$> TN.lookupTenant conn (AC.pageTokenHost pageToken)

withTokenAndTenant :: (AC.PageToken -> AC.TenantWithUser -> AppHandler ()) -> AppHandler ()
withTokenAndTenant processor = void $ withTenant $ \ct -> do
  token <- liftIO $ AC.generateTokenCurrentTime ct
  processor token ct

inSecond :: b -> a -> (a, b)
inSecond x y = (y, x)

