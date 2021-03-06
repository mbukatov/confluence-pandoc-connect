{-# LANGUAGE OverloadedStrings   #-}

module MicrosZone
    ( Zone(..)
    , zoneFromString
    , fromEnv
    , fromEnvDefaultDev
    , modifyDescriptorUsingZone
    ) where

import           Control.Monad           (join)
import qualified Data.Connect.Descriptor as D
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import qualified EnvironmentHelpers      as EH

data Zone = Dev | Dog | Prod
   deriving(Eq, Show, Ord)

zoneFromString :: String -> Maybe Zone
-- Development environments
zoneFromString "domain.dev.atlassian.io" = Just Dev
zoneFromString "ap-southeast-2.dev.public.atl-paas.net" = Just Dev
-- Dogfooding environments
zoneFromString "app.dev.atlassian.io" = Just Dog
zoneFromString "--app.ap-southeast-2.dev.public.atl-paas.net" = Just Dog
zoneFromString "--app.us-west-2.dev.public.atl-paas.net" = Just Dog
zoneFromString "useast.staging.atlassian.io" = Just Dog
zoneFromString "uswest.staging.atlassian.io"  = Just Dog
zoneFromString "us-east-1.staging.public.atl-paas.net" = Just Dog
zoneFromString "us-west-1.staging.public.atl-paas.net" = Just Dog
zoneFromString "us-west-2.staging.public.atl-paas.net" = Just Dog
zoneFromString "eu-west-1-beta.staging.public.atl-paas.net" = Just Dog
zoneFromString "eu-west-1.staging.public.atl-paas.net" = Just Dog
-- Production environments
zoneFromString "useast.atlassian.io" = Just Prod
zoneFromString "uswest.atlassian.io"  = Just Prod
zoneFromString "us-east-1.prod.public.atl-paas.net" = Just Prod
zoneFromString "us-west-1.prod.public.atl-paas.net" = Just Prod
zoneFromString "us-west-2.prod.public.atl-paas.net" = Just Prod
zoneFromString "eu-west-1-beta.prod.public.atl-paas.net" = Just Prod
zoneFromString "eu-west-1.prod.public.atl-paas.net" = Just Prod

zoneFromString _        = Nothing

fromEnv :: IO (Maybe Zone)
fromEnv = do
   envStr <- EH.getEnv "ZONE"
   return . join $ fmap zoneFromString envStr

fromEnvDefaultDev :: IO Zone
fromEnvDefaultDev = fmap (fromMaybe Dev) fromEnv

modifyDescriptorUsingZone :: Maybe Zone -> D.Plugin -> D.Plugin
modifyDescriptorUsingZone potentialZone descriptor = descriptor
    { D.pluginName = case D.pluginName descriptor of
        Nothing -> Nothing
        (Just (D.Name n)) -> Just . D.Name $ n `T.append` nameKeyAppend potentialZone
    , D.pluginKey = case D.pluginKey descriptor of (D.PluginKey k) -> D.PluginKey $ k `T.append` zoneKeyAppend potentialZone
    }

nameKeyAppend :: Maybe Zone -> T.Text
nameKeyAppend (Just Prod)   = ""
nameKeyAppend (Just zone)   = T.pack $ " (" ++ show zone ++ ")"
nameKeyAppend Nothing       = " (Local)"

zoneKeyAppend :: Maybe Zone -> T.Text
zoneKeyAppend (Just Prod)   = ""
zoneKeyAppend (Just Dog)    = ".dog"
zoneKeyAppend (Just Dev)    = ".dev"
zoneKeyAppend Nothing       = ".local"
