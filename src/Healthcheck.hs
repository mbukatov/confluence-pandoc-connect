{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Healthcheck
       ( heartbeatRequest
       , versionJson
       ) where

import           Application
import           Data.Aeson
import           Data.Int
import qualified Data.Text                          as T
import qualified Development.GitRev                 as GR
import           Distribution.PackageDescription.TH
import           GHC.Generics
import           Persistence.PostgreSQL
import           Persistence.Tenant                 (getTenantCount)
import           Snap.Core
import           SnapHelpers

data GitStuff = GitStuff
               { gitCommitHash :: T.Text
               , gitBranch     :: T.Text
               } deriving (Show, Eq, Generic)

data VersionJsonWrapping = VersionJsonWrapping
                           { gitStuff    :: GitStuff
                           , packageName :: T.Text
                           , version     :: T.Text
                           } deriving (Show, Eq, Generic)

data HealthStatus = HealthStatus
                    { healthy       :: Bool
                    , failureReason :: [T.Text]
                    } deriving (Show, Eq, Generic)
data Heartbeat = Heartbeat
                 { healthStatus :: HealthStatus
                 , tenantCount  :: Int64
                 } deriving (Show, Eq, Generic)

instance ToJSON HealthStatus
instance ToJSON GitStuff
instance ToJSON VersionJsonWrapping
instance ToJSON Heartbeat

heartbeatRequest :: AppHandler ()
heartbeatRequest = do
  tenantCount_ <- withConnection getTenantCount
  respondWith ok
  writeJson Heartbeat { healthStatus = HealthStatus True [], tenantCount = tenantCount_ }

versionJson :: AppHandler ()
versionJson = respondWith ok >> writeJson wrapped
   where
     wrapped = VersionJsonWrapping
       { gitStuff = GitStuff { gitCommitHash = $(GR.gitHash)
                             , gitBranch = $(GR.gitBranch)
                             }
       , packageName = $(packageVariable $ pkgName . package)
       , version = $(packageVariable $ pkgVersion . package)
       }
