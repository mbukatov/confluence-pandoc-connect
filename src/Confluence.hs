{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Confluence where

import           Application
import           ConfluenceTypes
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
import           Data.Map.Syntax
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as E
import           Data.Text.Lens                        (unpacked)
import           GHC.Generics
import           Heist
import qualified Heist.Interpreted                     as I
import           JsonRpc
import           Key
import           Network.HTTP.Client                   (Request,
                                                        RequestBody (..),
                                                        requestBody)
import           Network.HTTP.Client.MultipartFormData as MFD
import           Network.HTTP.Types.Header
import           Prelude
import           Snap.AtlassianConnect
import qualified Snap.AtlassianConnect.HostRequest     as HR
import           Snap.Core                             as SC
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileUploads
import qualified SnapHelpers                           as SH
import           Text.Pandoc
import           Text.Pandoc.MediaBag
import           WithToken


data ErrorMessage = ErrorMessage
  { emPageTitle :: Maybe T.Text
  , emMessage   :: T.Text
  } deriving (Show, Eq, Generic)
instance A.ToJSON ErrorMessage

baseUrlFromTenant :: Tenant -> T.Text
baseUrlFromTenant = T.pack . show . getURI . baseUrl

renderFileForm :: PageToken -> TenantWithUser -> AppHandler ()
renderFileForm token (tenant, _) = do
  connectData <- getConnect
  contentId <- paramValueFromUrl "content.id"
  spaceKey <- paramValueFromUrl "space.key"
  let
    connectPageToken = E.decodeUtf8 $ encryptPageToken (connectAES connectData) token
    splices = do
      "productBaseUrl" ## baseUrlFromTenant tenant
      "connectPageToken" ## connectPageToken
      "contentId" ## contentId
      "spaceKey" ## spaceKey
  heistLocal (I.bindStrings splices) $ render "file_form"
  where
    paramValueFromUrl name = do
      maybeValue <- SC.getParam name
      case maybeValue of
        Nothing -> fail "Required parameter missing"
        Just paramValue -> return $ SH.byteStringToText paramValue

errorResponse :: ErrorMessage -> TenantWithUser -> AppHandler ()
errorResponse message (tenant, _) = do
  putResponse $ setResponseCode SH.badRequest $ emptyResponse
  SH.writeJson message

createDirectoryPage :: T.Text -> ConfluenceTypes.Space -> Maybe ConfluenceTypes.PageId -> TenantWithUser -> AppHandler (Either HR.ProductErrorResponse A.Value)
createDirectoryPage title spaceKey maybePageId tenantWithUser =
  createPage title content spaceKey maybePageId tenantWithUser
  where
    content = "<p><ac:structured-macro ac:name=\"children\" ac:schema-version=\"2\" /></p>"

convertFileFromFormData :: AppHandler ()
convertFileFromFormData = do
  maybeFilenameAndContent : _ <- handleFileUploads
    "/tmp"
    uploadPolicy
    (const $ allowWithMaximumSize (getMaximumFormInputSize defaultUploadPolicy))
    handlePart

  maybeSpaceKey <- getParam "space-key"
  let maybeSpaceKey' = ConfluenceTypes.Space . Key . E.decodeUtf8 <$> maybeSpaceKey
  canCreate <- maybe (fail "No space key?!") (tenantFromToken . userCanCreatePage) maybeSpaceKey'
  if fromMaybe False canCreate
     then maybe uploadFailed (\(filename, fileContent) -> convertFile filename fileContent) maybeFilenameAndContent
     else (noPermission (T.pack . fst <$> maybeFilenameAndContent))
  where
    uploadPolicy = setMaximumFormInputSize maxFileSize defaultUploadPolicy
    maxFileSize = 100000000 -- 100 MB
    handlePart fileInfo errorOrFilePath = do
      if partFieldName fileInfo == "file-upload"
        then
            either
              (\_ -> return Nothing)
              (\filePath -> do
                fileContent <- BS.readFile filePath
                return . Just $ (BC.unpack . fromMaybe "empty file" $ partFileName fileInfo, fileContent)
              )
              errorOrFilePath
        else return Nothing
    uploadFailed = fromJust <$> tenantFromToken (errorResponse $ ErrorMessage Nothing "Uploading your file to the importer failed. Please try again.")
    noPermission name = fromJust <$> tenantFromToken (errorResponse $ ErrorMessage name "You don't have permission to create a new page in this space. If you think you should, please contact your administrator.")

convertFile :: String -> BS.ByteString -> AppHandler ()
convertFile filename fileContent =
  either (const noReaderFound) runReader $ readerFromFilename filename
  where
    packedFilename = Just . T.pack $ filename
    noReaderFound = fromJust <$> tenantFromToken (errorResponse $ ErrorMessage packedFilename "Unsupported file format. Please supply a file formatted any of markdown, reStructuredText, textile, HTML, DocBook, LaTeX, MediaWiki markup, TWiki markup, OPML, Emacs Org-Mode, Txt2Tags, Microsoft Word docx, EPUB, or Haddock markup.")
    readFailed = fromJust <$> tenantFromToken (errorResponse $ ErrorMessage packedFilename "We couldn't understand your file :(")
    pageCreateFailed = fromJust <$> tenantFromToken (errorResponse $ ErrorMessage packedFilename "File import failed: Couldn't create a new Confluence page.")

    runReader (StringReader readerF) = do
      let doRead = readerF def -- def is "default"
      errorOrReadResult <- liftIO $ doRead . BC.unpack $ fileContent
      either
        (const readFailed)
        (\pandoc -> do
            maybePageId <- (liftIO $ pandocToConfluenceStorageFormat pandoc) >>= writeConfluenceStorageFormat
            maybe pageCreateFailed (\_ -> return ()) maybePageId)
        errorOrReadResult

    runReader (ByteStringReader readerF) = do
      let doRead = readerF def
      errorOrReadResult <- liftIO . doRead $ LBS.fromStrict fileContent
      either
        (const readFailed)
        (\(pandoc, mediaBag) -> do
             maybePageId <- (liftIO $ pandocToConfluenceStorageFormat pandoc) >>= writeConfluenceStorageFormat
             resp <- maybe (return Nothing) (\pageId -> tenantFromToken $ uploadMedia pageId mediaBag) maybePageId
             maybe pageCreateFailed (either (const pageCreateFailed) (\_ -> return ())) resp
        )
        errorOrReadResult

uploadMedia :: PageId -> MediaBag -> TenantWithUser -> AppHandler (Either HR.ProductErrorResponse A.Value)
uploadMedia (PageId pageId) mediaBag (tenant, maybeUser) = do
  boundary <- liftIO webkitBoundary
  body <- liftIO $ renderParts boundary parts
  let request = hostPostRequest tenant (BC.pack attachmentUrl) []
                    $ HR.addHeader (hContentType, "multipart/form-data; boundary=" <> boundary) <>
                      HR.addHeader (CI.mk "X-Atlassian-Token", "nocheck") <>
                      Endo (\r -> r { requestBody = body })
  with connect request
  where
    attachmentUrl = "/rest/api/content/" ++ show pageId ++ "/child/attachment"
    allFiles = mapMaybe (\(path, _, _) -> (\(f, s) -> (path, f, s)) <$> lookupMedia path mediaBag) (mediaDirectory mediaBag)
    partTransform (path, mime, content) =
      (partFileRequestBody "file" path $ RequestBodyLBS content) { MFD.partContentType = Just $ BC.pack mime }
    parts = map partTransform allFiles

createPage :: T.Text -> T.Text -> ConfluenceTypes.Space -> Maybe ConfluenceTypes.PageId -> TenantWithUser -> AppHandler (Either HR.ProductErrorResponse A.Value)
createPage filename fileContent spaceKey maybePageId twu@(tenant, maybeUser) = do
  pageTitle_ <- getUniquePageName filename spaceKey twu
  let requestBody_ = A.encode PageDetails
                      { pageType = Page
                      , pageTitle = pageTitle_
                      , pageSpace = spaceKey
                      , pageBody = Body fileContent
                      , pageAncestors = maybeToList maybePageId
                      }
  with connect $ hostPostRequest tenant "/rest/api/content" []
                       $ HR.setBody (LBS.toStrict requestBody_) <>
                         HR.addHeader (hContentType, "application/json")

pandocToConfluenceStorageFormat :: Pandoc -> IO T.Text
pandocToConfluenceStorageFormat pandoc = T.pack <$> writeCustom "resources/confluence-storage.lua" def pandoc

getUniquePageName :: T.Text -> ConfluenceTypes.Space -> TenantWithUser -> AppHandler T.Text
getUniquePageName originalName spaceKey twu = do
  number <- getNumber originalName 0
  return $ originalName `T.append` numberSuffix number
  where
    getNumber :: T.Text -> Integer -> AppHandler Integer
    getNumber name number = do
      needNext <- pageExists (name `T.append` numberSuffix number) spaceKey twu
      if needNext then getNumber name (number + 1) else return number
    numberSuffix number = if number == 0 then "" else T.concat ["(", T.pack . show $ number, ")"]

pageExists :: T.Text -> ConfluenceTypes.Space -> TenantWithUser -> AppHandler Bool
pageExists name (ConfluenceTypes.Space (Key spaceKey)) (tenant, _) =
  contentReq >>= either (fail "Failed to talk to Confluence") (return . not . searchIsEmpty)
  where
    contentReq = with connect $ hostGetRequest tenant (BS.concat ["/rest/api/content?", "spaceKey=", E.encodeUtf8 spaceKey, "&title=", E.encodeUtf8 name]) [] mempty
    searchIsEmpty :: A.Value -> Bool
    searchIsEmpty val = val ^? A.key "size" . A._Number == Just 0

writeConfluenceStorageFormat :: T.Text -> AppHandler (Maybe PageId)
writeConfluenceStorageFormat text = do
  maybePageTitle <- getParam "page-title"
  maybeSpaceKey <- getParam "space-key"
  maybePageIdParam <- getParam "page-selectors"
  let maybePageId = parsePageIdParam maybePageIdParam
  putResponse $ setResponseCode 200 $ setContentType "text/html" emptyResponse
  errorOrResponse <- tenantFromToken $ createPage
                       (E.decodeUtf8 $ fromMaybe "no title" maybePageTitle)
                       text
                       (ConfluenceTypes.Space . Key . E.decodeUtf8 $ fromMaybe "" maybeSpaceKey)
                       maybePageId
  let pageId = join $ traverse (either (const Nothing) getPageId) errorOrResponse
  maybe (return ()) (either (\_ -> return ()) pageRedirect) errorOrResponse
  return pageId
  where
    getPageId :: A.Value -> Maybe PageId
    getPageId o = PageId <$> o ^? A.key "id" . A._String . unpacked . _Show
    parsePageIdParam :: Maybe BC.ByteString -> Maybe PageId
    parsePageIdParam x = PageId . fst <$> (x >>= BC.readInteger)

-- TODO handle error cases properly
pageRedirect :: A.Value -> AppHandler ()
pageRedirect o =
  maybe noLinkFound jsRedirect wu
  where
    links = o ^? A.key "_links"
    link s = links ^? folded . A.key s . A._String
    wu = link "base" <> link "webui"
    jsRedirect d =
      heistLocal (I.bindStrings $ "destination" ## d) $ render "page_redirect_dialog"
    noLinkFound = fromJust <$> tenantFromToken (errorResponse $ ErrorMessage Nothing "We couldn't figure out where Confluence created your page, please close the dialog manually.")

readerFromFilename :: String -> Either String Reader
readerFromFilename filename =
  getReader $ case suffix of
    "md" -> "markdown"
    "tex" -> "latex"
    "mw" -> "mediawiki"
    _ -> suffix
  where
    suffix = reverse $ takeWhile ('.' /=) $ reverse filename

hostPostRequest :: A.FromJSON a => Tenant -> BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Endo Network.HTTP.Client.Request -> Handler b Connect (Either HR.ProductErrorResponse a)
hostPostRequest t uri auth req = HR.hostPostRequest t uri auth req >>= (\r -> logProductError r >> return r)
  where
    logProductError (Left err) = logError $ BC.concat ["POST request to ", uri, " failed: ", BC.pack $ show err]
    logProductError _ = return ()

hostGetRequest :: A.FromJSON a => Tenant -> BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Endo Network.HTTP.Client.Request -> Handler b Connect (Either HR.ProductErrorResponse a)
hostGetRequest t uri auth req = HR.hostGetRequest t uri auth req >>= (\r -> logProductError r >> return r)
  where
    logProductError (Left err) = logError $ BC.concat ["GET request to ", uri, " failed: ", BC.pack $ show err]
    logProductError _ = return ()
