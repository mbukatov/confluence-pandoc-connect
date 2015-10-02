{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Confluence where

import           Application
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
import           Heist
import qualified Heist.Interpreted                     as I
import           Key
import           Network.HTTP.Client                   (RequestBody (..),
                                                        requestBody)
import           Network.HTTP.Client.MultipartFormData as MFD
import           Network.HTTP.Types.Header
import           Page
import           Prelude
import           Snap.AtlassianConnect
import qualified Snap.AtlassianConnect.HostRequest     as HR
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileUploads
import           Text.Pandoc
import           Text.Pandoc.MediaBag
import           WithToken

baseUrlFromTenant :: Tenant -> T.Text
baseUrlFromTenant = T.pack . show . getURI . baseUrl

renderFileForm :: PageToken -> TenantWithUser -> AppHandler ()
renderFileForm token (tenant, _) = do
  connectData <- getConnect
  let
    connectPageToken = E.decodeUtf8 $ encryptPageToken (connectAES connectData) token
    splices = do
      "productBaseUrl" ## baseUrlFromTenant tenant
      "connectPageToken" ## connectPageToken
  heistLocal (I.bindStrings splices) $ render "file_form"

renderErrorPage :: T.Text -> T.Text -> TenantWithUser -> AppHandler ()
renderErrorPage title content (tenant, _) =
  heistLocal (I.bindStrings splices) $ render "error_page"
  where
    splices = do
      "productBaseUrl" ## baseUrlFromTenant tenant
      "errorTitle" ## title
      "errorContent" ## content

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
    uploadFailed = fromJust <$> tenantFromToken (renderErrorPage "Upload failed" "Uploading your file to the importer failed. Please try again.")

convertFile :: String -> BS.ByteString -> AppHandler ()
convertFile filename fileContent =
  either (const noReaderFound) runReader $ readerFromFilename filename
  where
    noReaderFound = fromJust <$> tenantFromToken (renderErrorPage "Unsupported file format" "Please supply a file formatted any of markdown, reStructuredText, textile, HTML, DocBook, LaTeX, MediaWiki markup, TWiki markup, OPML, Emacs Org-Mode, Txt2Tags, Microsoft Word docx, EPUB, or Haddock markup.")
    readFailed = fromJust <$> tenantFromToken (renderErrorPage "File import failed" "We couldn't understand your file :(")
    pageCreateFailed = fromJust <$> tenantFromToken (renderErrorPage "File import failed" "Couldn't create a new Confluence page.")

    runReader (StringReader readerF) = do
      let doRead = readerF def -- def is "default"
      errorOrReadResult <- liftIO . doRead . BC.unpack $ fileContent
      either
        (const readFailed)
        (writeConfluenceStorageFormat >=> maybe pageCreateFailed (\_ -> return ()))
        errorOrReadResult

    runReader (ByteStringReader readerF) = do
      let doRead = readerF def
      errorOrReadResult <- liftIO . doRead $ LBS.fromStrict fileContent
      either
        (const readFailed)
        (\(pandoc, mediaBag) -> do
             maybePageId <- writeConfluenceStorageFormat pandoc
             resp <- maybe (return Nothing) (\pageId -> tenantFromToken $ uploadMedia pageId mediaBag) maybePageId
             maybe pageCreateFailed (either (const pageCreateFailed) (\_ -> return ())) resp
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
    partTransform (path, mime, content) =
      (partFileRequestBody "file" path $ RequestBodyLBS content) { MFD.partContentType = Just $ BC.pack mime }
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
  let pageId = join $ traverse (either (const Nothing) getPageId) errorOrResponse
  maybe (return ()) (either (\_ -> return ()) pageRedirect) errorOrResponse
  return pageId
  where
    getPageId :: A.Value -> Maybe PageId
    getPageId o = PageId <$> o ^? A.key "id" . A._String . unpacked . _Show

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
    noLinkFound = fromJust <$> tenantFromToken (renderErrorPage "Redirect failed" "We couldn't figure out where Confluence created your page, please close the dialog manually.")

readerFromFilename :: String -> Either String Reader
readerFromFilename filename =
  getReader $ case suffix of
    "md" -> "markdown"
    "tex" -> "latex"
    "mw" -> "mediawiki"
    _ -> suffix
  where
    suffix = reverse $ takeWhile ('.' /=) $ reverse filename
