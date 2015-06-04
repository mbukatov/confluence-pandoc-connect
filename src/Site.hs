{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Pandoc
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

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
  method POST (render "storage_formatted_response")

convertFile :: String -> String -> Handler App App ()
convertFile filename fileString = do
  let errorOrReader = getReader filename
  either
    readFailed
    (\(StringReader readerF)-> do
      let read = readerF def
      errorOrReadResult <- liftIO $ read fileString
      either (readFailed . show) writeConfluenceStorageFormat errorOrReadResult
    )
    errorOrReader
  where
    readFailed errorString = do
      putResponse $ setResponseCode 400 $ setContentType "text/plain" emptyResponse
      writeBS $ pack errorString

writeConfluenceStorageFormat :: Pandoc -> Handler App App ()
writeConfluenceStorageFormat pandoc = do
  writeResult <- liftIO $ writeCustom "resources/confluence-storage.lua" def pandoc
  putResponse $ setResponseCode 200 $ setContentType "text/html" emptyResponse
  heistLocal (I.bindString "storageFormatResponse" (T.pack writeResult)) $
    render "storage_formatted_response"
  return ()

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("rest/heartbeat", heartbeatRequest)
         , ("",          serveDirectory "static")
         , ("/atlassian-connect.json", serveDescriptor)
         , ("/create", handleCreateRequest)
         ]


------------------------------------------------------------------------------
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
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a

