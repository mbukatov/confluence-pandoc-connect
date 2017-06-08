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
import qualified Heist.Interpreted                     as I
import           JsonRpc
import           Key
import           Network.HTTP.Client                   (RequestBody (..),
                                                        requestBody)
import           Network.HTTP.Client.MultipartFormData as MFD
import           Network.HTTP.Types.Header
import           Network.URI
import           Prelude
import           Snap.AtlassianConnect
import qualified Snap.AtlassianConnect.HostRequest     as HR
import           Snap.Core                             as SC
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileUploads
import qualified SnapHelpers                           as SH
import           System.FilePath
import           Text.Pandoc
import           Text.Pandoc.MediaBag
import           WithToken


data ErrorMessage = ErrorMessage
  { emPageTitle :: Maybe T.Text
  , emMessage   :: T.Text
  } deriving (Show, Eq, Generic)
instance A.ToJSON ErrorMessage

data RedirectResponse = RedirectResponse
  { rrPageTitle :: Maybe T.Text
  , rrUri       :: URI
  } deriving (Show, Eq, Generic)
instance A.ToJSON RedirectResponse

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
        Nothing         -> fail "Required parameter missing"
        Just paramValue -> return $ SH.byteStringToText paramValue

errorResponse :: ErrorMessage -> AppHandler ()
errorResponse message = do
  putResponse $ setResponseCode SH.badRequest $ emptyResponse
  SH.writeJson message

createDirectoryPage :: T.Text -> ConfluenceTypes.Space -> Maybe ConfluenceTypes.PageId -> TenantWithUser -> AppHandler (Either HR.ProductErrorResponse A.Value)
createDirectoryPage title spaceKey maybePageId tenantWithUser =
  createPage title content spaceKey maybePageId tenantWithUser
  where
    content = "<p><ac:structured-macro ac:name=\"children\" ac:schema-version=\"2\" /></p>"

convertFileFromFormData :: AppHandler ()
convertFileFromFormData = do
  filenameAndContentOrError : _ <- handleFileUploads
    "/tmp"
    uploadPolicy
    (const $ allowWithMaximumSize (getMaximumFormInputSize uploadPolicy))
    handlePart

  maybeSpaceKey <- getParam "space-key"
  let maybeSpaceKey' = ConfluenceTypes.Space . Key . E.decodeUtf8 <$> maybeSpaceKey
  canCreate <- maybe (fail "No space key?!") (tenantFromToken . userCanCreatePage) maybeSpaceKey'
  if fromMaybe False canCreate
     then either uploadFailed (uncurry convertFile) filenameAndContentOrError
     else noPermission (T.pack . fst <$> filenameAndContentOrError)
  where
    uploadPolicy = setMaximumFormInputSize maxFileSize (setMaximumFormInputSize maxFileSize defaultUploadPolicy)
    maxFileSize = 100000000 -- 100 MB
    handlePart fileInfo errorOrFilePath =
      if partFieldName fileInfo == "file-upload"
        then
            either
              (return . Left . policyViolationExceptionReason)
              (\filePath -> do
                fileContent <- BS.readFile filePath
                return . Right $ (BC.unpack . fromMaybe "empty file" $ partFileName fileInfo, fileContent)
              )
              errorOrFilePath
        else
          return $ Left "Missing file-upload field in request"
    uploadFailed reason = errorResponse $ ErrorMessage Nothing $
      "Uploading your file to the importer failed. Please try again.\n\nError: " `T.append` reason
    noPermission name = errorResponse $ ErrorMessage (either (const Nothing) Just name)
      "You don't have permission to create a new page in this space. If you think you should, please contact your administrator."

convertFile :: String -> BS.ByteString -> AppHandler ()
convertFile filename fileContent =
  either (const noReaderFound) runReader $ readerFromFilename filename
  where
    packedFilename = Just . T.pack $ filename
    noReaderFound = errorResponse $ ErrorMessage packedFilename "Unsupported file format. Please supply a file formatted any of markdown, reStructuredText, textile, HTML, DocBook, LaTeX, MediaWiki markup, TWiki markup, OPML, Emacs Org-Mode, Txt2Tags, Microsoft Word docx, EPUB, or Haddock markup."
    readFailed = errorResponse $ ErrorMessage packedFilename "We couldn't understand your file :("
    pageCreateFailed = errorResponse $ ErrorMessage packedFilename "File import failed: Couldn't create a new Confluence page."

    runReader (StringReader readerF) = do
      let doRead = readerF def -- def is "default"
      errorOrReadResult <- liftIO $ doRead . BC.unpack $ fileContent
      either
        (const readFailed)
        (\pandoc -> liftIO (pandocToConfluenceStorageFormat pandoc) >>= writeConfluenceStorageFormat >> return ())
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
uploadMedia (PageId pageId) mediaBag (tenant, _) = do
  boundary <- liftIO webkitBoundary
  body <- liftIO $ renderParts boundary parts'
  let request = hostPostRequest tenant (BC.pack attachmentUrl) []
                    $ HR.addHeader (hContentType, "multipart/form-data; boundary=" <> boundary) <>
                      HR.addHeader (CI.mk "X-Atlassian-Token", "nocheck") <>
                      Endo (\r -> r { requestBody = body })
  with connect request
  where
    attachmentUrl = "/rest/api/content/" ++ show pageId ++ "/child/attachment"
    allFiles = mapMaybe (\(path', _, _) -> (\(f, s) -> (path', f, s)) <$> lookupMedia path' mediaBag) (mediaDirectory mediaBag)
    partTransform (path', mime, content) =
      (partFileRequestBody "file" path' $ RequestBodyLBS content) { MFD.partContentType = Just $ BC.pack mime }
    parts' = map partTransform allFiles

createPage :: T.Text -> T.Text -> ConfluenceTypes.Space -> Maybe ConfluenceTypes.PageId -> TenantWithUser -> AppHandler (Either HR.ProductErrorResponse A.Value)
createPage filename fileContent spaceKey maybePageId twu@(tenant, _) = do
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
      needNext <- isJust <$> pageExists (name `T.append` numberSuffix number) spaceKey twu
      if needNext then getNumber name (number + 1) else return number
    numberSuffix number = if number == 0 then "" else T.concat ["(", T.pack . show $ number, ")"]

pageExists :: T.Text -> ConfluenceTypes.Space -> TenantWithUser -> AppHandler (Maybe PageId)
pageExists name (ConfluenceTypes.Space (Key spaceKey)) (tenant, _) = do
  resp <- contentReq
  either
    (fail "Failed to talk to Confluence")
    (\v -> do
        let exists = not . searchIsEmpty $ v
            pageId = if exists then getResults v >>= getPageId else Nothing
        return pageId
    )
    resp
  where
    getResults :: A.Value -> Maybe A.Value
    getResults o = o ^? A.key "results" . A.nth 0
    contentReq = with connect $ hostGetRequest tenant (BS.concat ["/rest/api/content?", "spaceKey=", E.encodeUtf8 spaceKey, "&title=", E.encodeUtf8 name]) [] mempty
    searchIsEmpty :: A.Value -> Bool
    searchIsEmpty val = val ^? A.key "size" . A._Number == Just 0

writeConfluenceStorageFormat :: T.Text -> AppHandler (Maybe PageId)
writeConfluenceStorageFormat text = do
  maybeFilePath <- getParam "page-title"
  let decodedMaybeFilePath = E.decodeUtf8 <$> maybeFilePath
      stringFilePath = T.unpack <$> decodedMaybeFilePath
      maybePageTitle = takeFileName <$> stringFilePath
      maybePathPrefix = splitPath . dropFileNameNoDotSlash . normalise <$> stringFilePath
  maybeSpaceKey <- getParam "space-key"
  let spaceKey = ConfluenceTypes.Space . Key . E.decodeUtf8 $ fromMaybe "" maybeSpaceKey
  maybePageIdParam <- getParam "page-selectors"
  let maybePageId = parsePageIdParam maybePageIdParam
  putResponse $ setResponseCode 200 $ setContentType "text/html" emptyResponse
  let pageTitle' = fromMaybe "no title" $ T.pack <$> maybePageTitle
  finalParentPageId <- tenantFromToken $ createParents decodedMaybeFilePath (fromMaybe [] (map T.pack <$> maybePathPrefix)) spaceKey maybePageId
  errorOrResponse <- tenantFromToken $ createPage pageTitle' text spaceKey (join finalParentPageId)
  let pageId = join $ traverse (either (const Nothing) getPageId) errorOrResponse
  maybe (return ()) (either (\_ -> return ()) (pageRedirect $ E.decodeUtf8 <$> maybeFilePath)) errorOrResponse
  return pageId
  where
    parsePageIdParam :: Maybe BC.ByteString -> Maybe PageId
    parsePageIdParam x = PageId . fst <$> (x >>= BC.readInteger)
    createParents childName (name : names) spaceKey maybeParentPageId tenantWithUser = do
      parentExists <- pageExists name spaceKey tenantWithUser
      case parentExists of
        Just pageId -> createParents childName names spaceKey (Just pageId) tenantWithUser
        Nothing -> do
          errorOrResponse <- createDirectoryPage name spaceKey maybeParentPageId tenantWithUser
          either
            (\e -> errorResponse (ErrorMessage childName (HR.perMessage e)) >> return Nothing)
            (\resp ->
                maybe
                  (errorResponse (ErrorMessage childName "Failed to create a directory page") >> return Nothing)
                  (\pageId -> createParents childName names spaceKey (Just pageId) tenantWithUser)
                  (getPageId resp)
            )
            errorOrResponse
    createParents _ [] _ maybeParentPageId _ = return maybeParentPageId
    dropFileNameNoDotSlash s = let dropped = dropFileName s in if dropped == "./" then "" else dropped

getPageId :: A.Value -> Maybe PageId
getPageId o = PageId <$> o ^? A.key "id" . A._String . unpacked . _Show

-- TODO handle error cases properly
pageRedirect :: Maybe T.Text -> A.Value -> AppHandler ()
pageRedirect name o =
  maybe noLinkFound jsRedirect wu
  where
    links = o ^? A.key "_links"
    link s = links ^? folded . A.key s . A._String
    wu = link "base" <> link "webui" >>= parseURI . T.unpack
    jsRedirect d = SH.writeJson $ RedirectResponse name d
    noLinkFound = errorResponse $ ErrorMessage Nothing "We couldn't figure out where Confluence created your page, please close the dialog manually."

readerFromFilename :: String -> Either String Reader
readerFromFilename filename =
  getReader $ case suffix of
    "md"  -> "markdown"
    "tex" -> "latex"
    "mw"  -> "mediawiki"
    _     -> suffix
  where
    suffix = reverse $ takeWhile ('.' /=) $ reverse filename

