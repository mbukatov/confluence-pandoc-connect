{-# LANGUAGE OverloadedStrings #-}

module JsonRpc where

import           Application
import           Snap.AtlassianConnect
import Data.Aeson
import           Control.Lens hiding ((.=))
import qualified Data.Aeson.Lens                       as A
import           Data.Monoid
import ConfluenceTypes
import Key
import           Network.HTTP.Types.Header
import           Snap.Snaplet
import qualified Snap.AtlassianConnect.HostRequest     as HR

newtype RpcSpace = RpcSpace Space

data GetPermissionsForUser = GetPermissionsForUser
                             { gpfuSpace :: RpcSpace
                             , gpfuUser :: User
                             }

instance ToJSON RpcSpace where
  toJSON (RpcSpace (Space (Key k))) = String k
instance ToJSON User where
  toJSON (User (Key k)) = String k

getPermissionsForUser :: Space -> User -> GetPermissionsForUser
getPermissionsForUser s u = GetPermissionsForUser {gpfuSpace = RpcSpace s, gpfuUser = u}

instance ToJSON GetPermissionsForUser where
  toJSON o = object
    [ "jsonrpc" .= String "2.0"
    , "method" .= String "getPermissionsForUser"
    , "params" .= [toJSON $ gpfuSpace o, toJSON $ gpfuUser o]
    ]

userCanCreatePage :: Space -> TenantWithUser -> AppHandler Bool
userCanCreatePage space (tenant, maybeUser) = do
  user <- maybe (fail "No user found") return maybeUser
  resp <- with connect $ HR.hostPostRequest tenant "/rpc/json-rpc/confluenceservice-v2" []
                           $ HR.setBody (toJSON $ getPermissionsForUser space user) <>
                             HR.addHeader (hContentType, "application/json")
  return _

-- responseHasModifyPermission :: Value -> Boolean
-- responseHasModifyPermission o =
--   let
--     result = o ^? A.key "result"
--     result & catMaybes . toListOf folded . A.key "modify" . A._String
