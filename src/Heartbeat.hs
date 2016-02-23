{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Heartbeat
       ( heartbeatRequest
       ) where

import           Application
import           Data.Aeson
import qualified Data.Text          as T
import qualified Development.GitRev as GR (gitHash)
import           GHC.Generics
import           Snap.Core

data Version = Version
               { gitHash :: T.Text
               } deriving (Show, Eq, Generic)

instance ToJSON Version

heartbeatRequest :: AppHandler ()
heartbeatRequest = do
  putResponse $ setResponseCode 200 emptyResponse
  writeLBS . encode $ Version $(GR.gitHash)
