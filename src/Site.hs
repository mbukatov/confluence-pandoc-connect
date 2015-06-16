{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

import           Application
import           AtlassianConnect
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                             (ByteString,
                                                              readFile)
import           Data.ByteString.Char8                       (pack, unpack)
import qualified Data.ByteString.Lazy                        as LBS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as E
import           Heist
import qualified Heist.Interpreted                           as I
import           LifecycleHandlers
import           Prelude                                     hiding (readFile)
import           Snap.AtlassianConnect
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           Text.Pandoc

heartbeatRequest :: Handler App App ()
heartbeatRequest = putResponse $ setResponseCode 200 emptyResponse

serveDescriptor :: Handler App App ()
serveDescriptor = do
  let descriptor = "{\
\     \"name\": \"Confluence Pandoc Connect\",\
\     \"description\": \"Atlassian Connect add-on\",\
\     \"key\": \"io.atlassian.cpc\",\
\     \"baseUrl\": \"http://localhost:8000\",\
\     \"vendor\": {\
\         \"name\": \"Atlassian\",\
\         \"url\": \"http://www.atlassian.com\"\
\     },\
\     \"authentication\": {\
\         \"type\": \"none\"\
\     },\
\     \"apiVersion\": 1,\
\     \"modules\": {\
\         \"webItems\": [\
\             {\
\                 \"url\": \"/create\",\
\                 \"key\": \"pandoc-import\",\
\                 \"location\": \"system.content.action\",\
\                 \"name\": {\
\                     \"value\": \"Import from file\"\
\                 },\
\                 \"target\": {\
\                     \"type\": \"dialog\"\
\                 }\
\             }\
\         ]\
\     }\
\ }"
  putResponse $ setResponseCode 200 $ setContentType "application/json" emptyResponse
  writeBS descriptor

handleCreateRequest :: Handler App App ()
handleCreateRequest =
  method GET (render "file_form") <|>
  method POST convertFileFromFormData

convertFileFromFormData :: Handler App App ()
convertFileFromFormData = do
  maybeFilenameAndContent <- handleFileUploads "/tmp" uploadPolicy (\_ -> allowWithMaximumSize maxFileSize) formHandler
  maybe uploadFailed (\(filename, fileContent) -> convertFile filename fileContent) maybeFilenameAndContent
  where
    uploadPolicy = setMaximumFormInputSize maxFileSize defaultUploadPolicy
    maxFileSize = 100000000 -- 100 MB
    formHandler = foldl handlePart (return Nothing)
    handlePart acc (fileInfo, errorOrFilePath) = do
      prevResult <- acc
      if partFieldName fileInfo == "file-upload"
        then
            either
              (\_ -> uploadFailed >> return Nothing)
              (\filePath -> do
                fileContent <- liftIO $ readFile filePath
                return . Just $ (unpack . fromJust $ partFileName fileInfo, fileContent)
              )
              errorOrFilePath
        else return Nothing
    uploadFailed = do
      putResponse $ setResponseCode 400 $ setContentType "text/plain" emptyResponse
      writeBS "File upload failed"

convertFile :: String -> ByteString -> Handler App App ()
convertFile filename fileContent = do
  let errorOrReader = readerFromFilename filename
  either readFailed runReader errorOrReader
  where
    readFailed errorString = do
      putResponse $ setResponseCode 400 $ setContentType "text/plain" emptyResponse
      writeBS $ pack errorString
    runReader (StringReader readerF) = do
      let read = readerF def
      errorOrReadResult <- liftIO . read . unpack $ fileContent
      either (readFailed . show) writeConfluenceStorageFormat errorOrReadResult
    runReader (ByteStringReader readerF) = do
      let read = readerF def
      errorOrReadResult <- liftIO . read $ LBS.fromStrict fileContent
      either (readFailed . show) writeConfluenceStorageFormat $ fmap fst errorOrReadResult

readerFromFilename :: String -> Either String Reader
readerFromFilename filename =
  getReader $ case suffix of
    "md" -> "markdown"
    "tex" -> "latex"
    "mw" -> "mediawiki"
    _ -> suffix
  where
    suffix = drop 1 $ dropWhile ('.' /=) filename

writeConfluenceStorageFormat :: Pandoc -> Handler App App ()
writeConfluenceStorageFormat pandoc = do
  maybePageTitle <- getParam "page-title"
  writeResult <- liftIO $ writeCustom "resources/confluence-storage.lua" def pandoc
  putResponse $ setResponseCode 200 $ setContentType "text/html" emptyResponse
  let splices = do
        "newPageTitle" ##  E.decodeUtf8 $ fromJust maybePageTitle
        "storageFormatResponse" ## T.pack writeResult
  heistLocal (I.bindStrings splices) $ render "storage_formatted_response"
  return ()

-- | The application's routes.
routes, applicationRoutes :: [(ByteString, Handler App App ())]
routes = applicationRoutes ++ lifecycleRoutes
applicationRoutes =
  [ ("rest/heartbeat", heartbeatRequest)
  , ("/atlassian-connect.json", serveDescriptor)
  , ("/create", handleCreateRequest)
  , ("/all.js", serveFile "resources/all.js")
  ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    db' <- nestSnaplet "db" db pgsInit
    ac <- nestSnaplet "connect" connect $ initConnectSnaplet addonDescriptor
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a db' ac

