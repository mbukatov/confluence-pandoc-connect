{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect
  ( addonDescriptor
  ) where

import           Data.Connect.Descriptor
import           Data.Maybe
import           Network.URI

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

addonDescriptor :: Plugin
addonDescriptor =
  basePlugin
    { pluginName      = Just . Name $ "Confluence Pandoc Connect"
    , pluginDescription  = Just "Import files into Confluence with the Pandoc document converter"
    , vendor         = Just $ Vendor (Name "Atlassian") atlassianHomepage
    , lifecycle = Just $ defaultLifecycle
        { enabled = Nothing
        , disabled = Nothing
        }
    -- TODO add confluence webItem module once it's available post CPC-20
    , modules = Just $ Modules emptyJIRAModules emptyConfluenceModules
    , scopes = Just [Read, Write]
    , enableLicensing = Just False
    }
  where
    basePlugin = pluginDescriptor (PluginKey "com.atlassian.confluence.pandocconnect") nullURI jwtAuthentication
    jwtAuthentication = Authentication Jwt

