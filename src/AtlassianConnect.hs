{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect
  ( addonDescriptor
  ) where

import           Data.Connect.Descriptor
import           Data.Maybe
import           Data.Text               as T
import           Network.URI

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

addonDescriptor :: Plugin
addonDescriptor =
  basePlugin
    { pluginName      = Just . Name $ "Confluence Pandoc Connect"
    , pluginDescription  = Just "Import files into Confluence with the Pandoc document converter."
    , vendor         = Just $ Vendor (Name "Atlassian") atlassianHomepage
    , lifecycle = Just $ defaultLifecycle
        { enabled = Nothing
        , disabled = Nothing
        }
    -- TODO add confluence webItem module once it's available post CPC-20
    , modules = Just $ Modules emptyJIRAModules emptyConfluenceModules
        { confluenceWebItems = Just [importDocumentMenuItem]
        }
    , scopes = Just [Read, Write]
    , enableLicensing = Just False
    }
  where
    basePlugin = pluginDescriptor (PluginKey "com.atlassian.confluence.pandocconnect") nullURI jwtAuthentication
    jwtAuthentication = Authentication Jwt

addonKeySuffix :: Text
addonKeySuffix = "-confluence-pandoc-connect"

importDocumentMenuItem :: WebItem
importDocumentMenuItem = WebItem
  { wiKey = suffix "import-document"
  , wiName = simpleText "Import from file"
  , wiLocation = "system.content.action"
  , wiUrl = "/create"
  , wiTooltip = Nothing
  , wiIcon = Nothing
  , wiWeight = Nothing
  , wiTarget = Just $ TargetDialog Nothing
  , wiStyleClasses = []
  , wiContext = Just AddonContext
  , wiConditions = [] -- TODO(CPC-21) only show if the user can create pages
  , wiParams = noParams
  }
  where
    suffix = flip T.append addonKeySuffix
