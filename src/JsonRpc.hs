{-# LANGUAGE OverloadedStrings #-}

module JsonRpc where

import           Application
import           ConfluenceTypes
import           Control.Lens                      hiding ((.=))
import           Data.Aeson
import qualified Data.Aeson.Lens                   as A
import qualified Data.ByteString.Char8             as BC
import qualified Data.ByteString.Lazy              as LBS
import           Data.Monoid
import           Key
import           Network.HTTP.Types.Header
import           Snap.AtlassianConnect
import qualified Snap.AtlassianConnect.HostRequest as HR
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

userCanCreatePage :: Space -> TenantWithUser -> AppHandler Bool
userCanCreatePage space twu@(tenant, maybeUser) = do
  user <- maybe (fail "No user found") return maybeUser
  username <- getUsername twu
  let body = LBS.toStrict . encode $ getPermissionsForUser space username
  resp <- with connect $ HR.hostPostRequest tenant "/rpc/json-rpc/confluenceservice-v2" []
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

getUsername :: TenantWithUser -> AppHandler Username
getUsername (tenant, maybeUser) = do
  user <- maybe (fail "No user found") return maybeUser
  let body = LBS.toStrict . encode $ GetUserByKey user
  resp <- with connect $ HR.hostPostRequest tenant "/rpc/json-rpc/confluenceservice-v2" []
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
