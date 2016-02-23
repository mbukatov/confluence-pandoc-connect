{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Healthcheck
       ( heartbeatRequest
       , versionJson
       ) where

import           Application
import           Data.Aeson
import qualified Data.Text                          as T
import qualified Development.GitRev                 as GR
import           Distribution.PackageDescription.TH
import           GHC.Generics
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

instance ToJSON GitStuff
instance ToJSON VersionJsonWrapping

heartbeatRequest :: AppHandler ()
heartbeatRequest = versionJson

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
