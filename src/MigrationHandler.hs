{-# LANGUAGE OverloadedStrings #-}

module MigrationHandler
   (migrationRequest
   ) where

import           Application
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import qualified EnvironmentHelpers     as DE
import           Finder
import           Key
import qualified Snap.Core              as SC
import qualified SnapHelpers            as SH
import           System.FilePath        ((</>))
import           System.Process         (callProcess)
import           Text.Read              (readMaybe)


newtype MigrationKey = MigrationKey (Key MigrationKey)
  deriving (Eq, Show)

migrationKeyName :: String
migrationKeyName = "FLYWAY_MIGRATION_KEY"

migrationRequest :: AppHandler ()
migrationRequest = SH.handleMethods
   [ (SC.PUT, handleMigrationRun)
   ]

-- This should be an idempotent operation, this means that we should require that the user pass in
-- the ID of the migration that they wish to run up to.
handleMigrationRun :: AppHandler ()
handleMigrationRun = do
  systemKey <- liftIO $ DE.getEnv migrationKeyName
  maybe (fail $ "Missing " ++ migrationKeyName) (getKeyAndConfirm handleFlywayMigrate)
    (MigrationKey . Key . T.pack <$> systemKey)

handleFlywayMigrate :: AppHandler ()
handleFlywayMigrate = either (SH.respondWithError SH.badRequest) (liftIO . flywayMigrate) =<< getFlywayOptions

flywayMigrate :: FlywayOptions -> IO ()
flywayMigrate options = do
   potentialFlywayPath <- findFile addFlywayPath
   case potentialFlywayPath of
      Just flywayPath -> callProcess flywayPath migrationArguments
      Nothing -> fail "Could not find the flyway executable relative to the running executables path."
   where
      migrationArguments = "migrate" : flywayOptionsToArguments options

addFlywayPath :: FilePath -> FilePath
addFlywayPath f = f </> "migrations" </> "flyway"

getFlywayOptions :: AppHandler (Either String FlywayOptions)
getFlywayOptions = do
   potentialTarget <- (readMaybe . BSC.unpack =<<) <$> SC.getParam (BSC.pack "target")
   case potentialTarget of
      Nothing -> return . Left $ "You need to provide a 'target' schema version param to the migration endpoint."
      Just target -> do
         pHost     <- siGetEnv  $ pgPre "HOST"
         pPort     <- (readMaybe =<<) <$> siGetEnv (pgPre "PORT") :: AppHandler (Maybe Integer)
         pSchema   <- siGetEnv  $ pgPre "SCHEMA"
         pRole     <- siGetEnv  $ pgPre "ROLE"
         pPassword <- siGetEnv  $ pgPre "PASSWORD"
         case (pHost, pPort, pSchema, pRole, pPassword) of
            (Just host, Just port, Just schema, Just role, Just password) -> do
               let connectionString = "jdbc:postgresql://" ++ host ++ ":" ++ show port ++ "/" ++ schema
               return . Right $ FlywayOptions
                  { flywayUrl = connectionString
                  , flywayUser = role
                  , flywayPassword = password
                  , flywayTarget = target
                  }
            _ -> return . Left $ "Could not load the database details from the environment variables."
   where
      siGetEnv :: String -> AppHandler (Maybe String)
      siGetEnv = liftIO . DE.getEnv

      pgPre :: String -> String
      pgPre = (++) "PG_CONFLUENCE_PANDOC_CONNECT_"

data FlywayOptions = FlywayOptions
   { flywayUrl      :: String
   , flywayUser     :: String
   , flywayPassword :: String
   , flywayTarget   :: Integer
   } deriving (Eq)

flywayOptionsToArguments :: FlywayOptions -> [String]
flywayOptionsToArguments fo =
   [ "-url=" ++ flywayUrl fo
   , "-user=" ++ flywayUser fo
   , "-password=" ++ flywayPassword fo
   , "-target=" ++ (show . flywayTarget $ fo)
   ]

getKeyAndConfirm :: AppHandler () -> MigrationKey -> AppHandler ()
getKeyAndConfirm success systemKey =
  SC.getPostParam "key" >>=
    maybe
       (SH.respondWithError SH.forbidden "Speak friend and enter. However: http://i.imgur.com/fVDH5bN.gif")
       (\requestKey -> if (MigrationKey . Key . E.decodeUtf8 $ requestKey) == systemKey
           then success
           else SH.respondWithError SH.forbidden "You lack the required permissions.")

