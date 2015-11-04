{-# LANGUAGE OverloadedStrings #-}

module JsonRpc where

import           Application
import           ConfluenceTypes
import           Control.Lens                      hiding ((.=))
import           Data.Aeson
import qualified Data.Aeson.Lens                   as A
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
                             , gpfuUser  :: UserKey
                             }

instance ToJSON RpcSpace where
  toJSON (RpcSpace (Space (Key k))) = String k

getPermissionsForUser :: Space -> UserKey -> GetPermissionsForUser
getPermissionsForUser s u = GetPermissionsForUser {gpfuSpace = RpcSpace s, gpfuUser = u}

instance ToJSON GetPermissionsForUser where
  toJSON o = object
    [ "jsonrpc" .= String "2.0"
    , "method" .= String "getPermissionsForUser"
    , "params" .= [toJSON $ gpfuSpace o, toJSON $ gpfuUser o]
    , "id" .= String "1" -- value doesn't matter to us
    ]

userCanCreatePage :: Space -> TenantWithUser -> AppHandler Bool
userCanCreatePage space (tenant, maybeUser) = do
  user <- maybe (fail "No user found") return maybeUser
  resp <- with connect $ HR.hostPostRequest tenant "/rpc/json-rpc/confluenceservice-v2" []
                           $ HR.setBody (LBS.toStrict . encode $ getPermissionsForUser space user) <>
                             HR.addHeader (hContentType, "application/json")
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
