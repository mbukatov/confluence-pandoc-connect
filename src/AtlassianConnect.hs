{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module AtlassianConnect
  ( addonDescriptor
  , ConnectFeatures(..)
  ) where

import           Data.Connect.Descriptor
import qualified Data.HashMap.Strict                   as HM
import           Data.Maybe
import           Data.Text                             as T
import           Network.URI
import           Prelude

data ConnectFeatures = ConnectFeatures
  { webItemDisplayEnabled :: Bool
  }

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

addonDescriptor :: ConnectFeatures -> Plugin
addonDescriptor cf =
  basePlugin
    { pluginName      = Just . Name $ "Confluence universal importer"
    , pluginDescription  = Just "Import files into Confluence with the Pandoc document converter."
    , vendor         = Just $ Vendor (Name "Atlassian") atlassianHomepage
    , lifecycle = Just $ defaultLifecycle
        { enabled = Nothing
        , disabled = Nothing
        }
    -- TODO add confluence webItem module once it's available post CPC-20
    , modules = Just $ Modules emptyJIRAModules emptyConfluenceModules
        { confluenceWebItems = Just [importDocumentMenuItem cf]
        }
    , scopes = Just [Read, Write, SpaceAdmin]
    , enableLicensing = Just False
    }
  where
    basePlugin = pluginDescriptor (PluginKey "com.atlassian.confluence.pandocconnect") nullURI jwtAuthentication
    jwtAuthentication = Authentication Jwt

addonKeySuffix :: Text
addonKeySuffix = "-confluence-pandoc-connect"

importDocumentMenuItem :: ConnectFeatures -> WebItem
importDocumentMenuItem cf = WebItem
  { wiKey = suffix "import-document"
  , wiName = simpleText "Import from file"
  , wiLocation = "system.content.action"
  , wiUrl = "/create?content.id={content.id}&space.key={space.key}"
  , wiTooltip = Nothing
  , wiIcon = Nothing
  , wiWeight = Nothing
  , wiTarget = Just $ TargetDialog Nothing
  , wiStyleClasses = []
  , wiContext = Just AddonContext
  , wiConditions = if webItemDisplayEnabled cf
                      then [ SingleCondition (StaticConfluenceCondition CreateContentConfluenceCondition) False (HM.singleton "content" "page") ]
                      else []
  , wiParams = noParams
  }
  where
    suffix = flip T.append addonKeySuffix
