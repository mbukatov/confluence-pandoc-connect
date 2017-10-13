{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonRpc where

import           Application
import           ConfluenceTypes
import           Control.Applicative
import           Control.Lens                      hiding ((.=))
import           Control.Monad
import           Data.Aeson                        as A
import qualified Data.Aeson.Lens                   as A
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import           Data.Either.Utils
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           GHC.Generics
import           Key
import           Network.HTTP.Client               (Request)
import           Network.HTTP.Types.Header
import           Prelude
import           Snap.AtlassianConnect
import qualified Snap.AtlassianConnect.HostRequest as HR
import           Snap.Logging.Json
import           Snap.Snaplet

newtype RpcSpace = RpcSpace Space

data GetPermissionsForUser = GetPermissionsForUser
                             { gpfuSpace :: RpcSpace
                             , gpfuUser  :: Username
                             }

instance ToJSON RpcSpace where
  toJSON (RpcSpace (Space (Key k))) = String k
instance ToJSON Username where
  toJSON (Username u) = String u

getPermissionsForUser :: Space -> Username -> GetPermissionsForUser
getPermissionsForUser s u = GetPermissionsForUser {gpfuSpace = RpcSpace s, gpfuUser = u}

instance ToJSON GetPermissionsForUser where
  toJSON o = object
    [ "jsonrpc" .= String "2.0"
    , "method" .= String "getPermissionsForUser"
    , "params" .= [toJSON $ gpfuSpace o, toJSON $ gpfuUser o]
    , "id" .= String "1" -- value doesn't matter to us
    ]

data GetUserByKey = GetUserByKey UserKey

instance ToJSON GetUserByKey where
  toJSON (GetUserByKey k) = object
    [ "jsonrpc" .= String "2.0"
    , "method" .= String "getUserByKey"
    , "params" .= [String k]
    , "id" .= String "2" -- value doesn't matter to us
    ]

data ProductErrorLog = ProductErrorLog
  { requestType         :: T.Text
  , productUri          :: T.Text
  , productResponseCode :: Int
  , productError        :: Either T.Text A.Object
  } deriving (Show, Generic)

instance ToJSON HR.ProductErrorResponse
instance ToJSON ProductErrorLog

userCanCreatePage :: Space -> TenantWithUser -> Handler b App Bool
userCanCreatePage space twu@(tenant, maybeUser) = do
  _ <- maybe (fail "No user found") return maybeUser
  username <- getUsername twu
  let body = LBS.toStrict . encode $ getPermissionsForUser space username
  resp <- hostPostRequest tenant "/rpc/json-rpc/confluenceservice-v2" []
                           $ HR.addHeader (hContentType, "application/json") <>
                             HR.setBody body
  either (\_ -> fail "Couldn't get user permissions from the application")
         (return . responseHasModifyPermission)
         resp

responseHasModifyPermission :: Value -> Bool
responseHasModifyPermission o =
  let
    result = o ^? A.key "result"
    perms r = r ^.. A.values . A._String
    hasModify ps = "modify" `elem` ps
  in maybe False (hasModify . perms) result

getUsername :: TenantWithUser -> Handler b App Username
getUsername (tenant, maybeUser) = do
  user <- maybe (fail "No user found") return maybeUser
  let body = LBS.toStrict . encode $ GetUserByKey user
  resp <- hostPostRequest tenant "/rpc/json-rpc/confluenceservice-v2" []
                           $ HR.addHeader (hContentType, "application/json") <>
                             HR.setBody body
  either (\_ -> fail "Couldn't get username from the application")
         (maybe (fail "Couldn't get the username from the application") return . usernameFromResponse)
         resp

usernameFromResponse :: Value -> Maybe Username
usernameFromResponse o = let
  result = o ^? A.key "result"
  name = foldMap (\r -> r ^? A.key "name" . A._String) result
  in Username <$> name

hostPostRequest :: A.FromJSON a => Tenant -> BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Endo Network.HTTP.Client.Request -> Handler b App (Either HR.ProductErrorResponse a)
hostPostRequest t uri auth req = do
  r <- with connect $ HR.hostPostRequest t Nothing uri auth req
  logProductError r
  return r
  where
    logProductError (Left err) = with logging . logJson $ encodeProductErrorLog "POST" uri err
    logProductError _ = return ()

hostGetRequest :: A.FromJSON a => Tenant -> BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Endo Network.HTTP.Client.Request -> Handler b App (Either HR.ProductErrorResponse a)
hostGetRequest t uri auth req = do
  r <- with connect $ HR.hostGetRequest t Nothing uri auth req
  logProductError r
  return r
  where
    logProductError (Left err) = with logging . logJson $ encodeProductErrorLog "GET" uri err
    logProductError _ = return ()

encodeProductErrorLog :: T.Text -> BS.ByteString -> HR.ProductErrorResponse -> ProductErrorLog
encodeProductErrorLog rt pu per = ProductErrorLog
      { requestType = rt
      , productUri = TE.decodeUtf8 pu
      , productResponseCode = HR.perCode per
      , productError = maybeToEither (HR.perMessage per) (A.decodeStrict . TE.encodeUtf8 $ HR.perMessage per)
      }
