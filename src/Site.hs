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
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                            as A
import qualified Data.Aeson.Lens                       as A
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.CaseInsensitive                  as CI
import           Data.Foldable                         (traverse_)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as E
import           Data.Text.Lens                        (unpacked)
import           Data.Version                          (showVersion)
import           Heist
import qualified Heist.Interpreted                     as I
import           Key
import           LifecycleHandlers
import           Network.HTTP.Client                   (RequestBody (..),
                                                        requestBody)
import           Network.HTTP.Client.MultipartFormData as MFD
import           Network.HTTP.Types.Header
import           Page
import           Paths_confluence_pandoc_connect       (version)
import           Prelude
import           Snap.AtlassianConnect
import qualified Snap.AtlassianConnect.HostRequest     as HR
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileUploads
import           System.Environment                    (getEnv)
import           TenantJWT
import           Text.Pandoc
import           Text.Pandoc.MediaBag
import           WithToken

heartbeatRequest :: AppHandler ()
heartbeatRequest = do
  putResponse $ setResponseCode 200 emptyResponse
  writeText . T.pack $ showVersion version

handleCreateRequest :: AppHandler ()
handleCreateRequest =
  method GET (withTokenAndTenant renderFileForm) <|>
  method POST convertFileFromFormData

renderFileForm :: PageToken -> TenantWithUser -> AppHandler ()
renderFileForm token (tenant, _) = do
  connectData <- getConnect
  let
    productBaseUrl = T.pack $ show $ getURI $ baseUrl tenant
    connectPageToken = E.decodeUtf8 $ encryptPageToken (connectAES connectData) token
    splices = do
      "productBaseUrl" ## productBaseUrl
      "connectPageToken" ## connectPageToken
  heistLocal (I.bindStrings splices) $ render "file_form"

withTokenAndTenant :: (PageToken -> TenantWithUser -> AppHandler ()) -> AppHandler ()
withTokenAndTenant processor = withTenant $ \ct -> do
  token <- liftIO $ generateTokenCurrentTime ct
  processor token ct

convertFileFromFormData :: AppHandler ()
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
                fileContent <- liftIO $ BS.readFile filePath
                return . Just $ (BC.unpack . fromMaybe "empty file" $ partFileName fileInfo, fileContent)
              )
              errorOrFilePath
        else return Nothing
    uploadFailed = do
      putResponse $ setResponseCode 400 $ setContentType "text/plain" emptyResponse
      writeBS "File upload failed"

convertFile :: String -> BS.ByteString -> AppHandler ()
convertFile filename fileContent = do
  let errorOrReader = readerFromFilename filename
  either readFailed runReader errorOrReader
  where
    readFailed errorString = do
      putResponse $ setResponseCode 400 $ setContentType "text/plain" emptyResponse
      writeBS $ BC.pack errorString
    runReader (StringReader readerF) = do
      let read = readerF def
      errorOrReadResult <- liftIO . read . BC.unpack $ fileContent
      either (readFailed . show) (void . writeConfluenceStorageFormat) errorOrReadResult
    runReader (ByteStringReader readerF) = do
      let read = readerF def
      errorOrReadResult <- liftIO . read $ LBS.fromStrict fileContent
      either
        (readFailed . show)
        (\(pandoc, mediaBag) -> do
             pageId <- fromJust <$> writeConfluenceStorageFormat pandoc
             resp <- tenantFromToken $ uploadMedia pageId mediaBag -- TODO handle attachment upload failure
             logError . BC.pack $ "upload reponse: " ++ show resp
             return ()
        )
        errorOrReadResult

uploadMedia :: PageId -> MediaBag -> TenantWithUser -> AppHandler (Either HR.ProductErrorResponse A.Value)
uploadMedia (PageId pageId) mediaBag (tenant, maybeUser) = do
  boundary <- liftIO webkitBoundary
  body <- liftIO $ renderParts boundary parts
  let request = HR.hostPostRequest tenant (BC.pack attachmentUrl) []
                    $ HR.addHeader (hContentType, "multipart/form-data; boundary=" <> boundary) <>
                      HR.addHeader (CI.mk "X-Atlassian-Token", "nocheck") <>
                      Endo (\r -> r { requestBody = body })
  with connect request
  where
    attachmentUrl = "/rest/api/content/" ++ show pageId ++ "/child/attachment"
    allFiles = mapMaybe (\(path, _, _) -> (\(f, s) -> (path, f, s)) <$> lookupMedia path mediaBag) (mediaDirectory mediaBag)
    partTransform (path, mime, content) = (partFileRequestBody "file" path $ RequestBodyLBS content) { MFD.partContentType = Just $ BC.pack mime }
    parts = map partTransform allFiles

createPage :: T.Text -> T.Text -> Page.Space -> TenantWithUser -> AppHandler (Either HR.ProductErrorResponse A.Value)
createPage filename fileContent spaceKey (tenant, maybeUser) = do
  let requestBody = A.encode PageDetails
                      { pageType = Page
                      , pageTitle = filename
                      , pageSpace = spaceKey
                      , pageBody = Body fileContent
                      }
  with connect $ HR.hostPostRequest tenant "/rest/api/content" []
                       $ HR.setBody (LBS.toStrict requestBody) <>
                         HR.addHeader (hContentType, "application/json")

readerFromFilename :: String -> Either String Reader
readerFromFilename filename =
  getReader $ case suffix of
    "md" -> "markdown"
    "tex" -> "latex"
    "mw" -> "mediawiki"
    _ -> suffix
  where
    suffix = reverse $ takeWhile ('.' /=) $ reverse filename

writeConfluenceStorageFormat :: Pandoc -> AppHandler (Maybe PageId)
writeConfluenceStorageFormat pandoc = do
  maybePageTitle <- getParam "page-title"
  maybeSpaceKey <- getParam "space-key"
  writeResult <- liftIO $ writeCustom "resources/confluence-storage.lua" def pandoc
  putResponse $ setResponseCode 200 $ setContentType "text/html" emptyResponse
  errorOrResponse <- tenantFromToken $ createPage
                       (E.decodeUtf8 $ fromMaybe "no title" maybePageTitle)
                       (T.pack writeResult)
                       (Page.Space . Key . E.decodeUtf8 $ fromMaybe "" maybeSpaceKey)
  -- TODO handle error cases properly
  let pageId = join $ traverse (either (const Nothing) getPageId) errorOrResponse
  traverse_ (either writeShow pageRedirect) errorOrResponse
  return pageId
  where
    getPageId :: A.Value -> Maybe PageId
    getPageId o = PageId <$> o ^? A.key "id" . A._String . unpacked . _Show

writeShow :: Show a => a -> AppHandler ()
writeShow = writeText . T.pack . show

-- TODO handle error cases properly
pageRedirect :: A.Value -> AppHandler ()
pageRedirect o = do
  maybe (writeText "Sorry, your upload failed") jsRedirect wu
  where
    links = o ^? A.key "_links"
    link s = links ^? folded . A.key s . A._String
    wu = link "base" <> link "webui"
    jsRedirect d =
      heistLocal (I.bindStrings $ "destination" ## d) $ render "page_redirect_dialog"

-- | The application's routes.
routes, applicationRoutes :: [(BS.ByteString, AppHandler ())]
routes = applicationRoutes ++ lifecycleRoutes
applicationRoutes =
  [ ("rest/heartbeat", heartbeatRequest)
  , ("/create", handleCreateRequest)
  ]

dbConfigFromEnv :: IO PGSConfig
dbConfigFromEnv = pgsDefaultConfig . BC.pack <$> getEnv "PG_CONFLUENCE_PANDOC_CONNECT_URL"

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "confluence-pandoc-connect" "CPC connect application" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    dbConfig <- liftIO dbConfigFromEnv
    db' <- nestSnaplet "db" db $ pgsInit' dbConfig
    ac <- nestSnaplet "connect" connect $ initConnectSnaplet addonDescriptor
    addRoutes routes
    return $ App h db' ac

