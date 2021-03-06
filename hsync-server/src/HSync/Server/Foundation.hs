{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Server.Foundation where

import           Prelude

import           Control.Applicative((<$>))
import           Control.Concurrent.STM.TChan

import           Data.Monoid

import           HSync.Common.Types
import           HSync.Common.Notification

import           Data.Text(pack, unpack)

import           HSync.Server

import           HSync.Server.LocalAuth (YesodLocalAuth(..), localAuth, loginR, registerR)
import           HSync.Server.Settings (widgetFile, Extra (..))
import           HSync.Server.Settings.StaticFiles
import           HSync.Server.Settings.Development (development)
import           HSync.Server.AcidState
import           HSync.Server.AcidSync
import           HSync.Server.FileSystemState(FSState, newUserFSState)
import           HSync.Server.User (User(..),UserIndex)

import           Network.HTTP.Conduit (Manager)

import           System.Directory(createDirectory)

import           Text.Jasmine (minifym)
import           Text.Hamlet (hamletFile)


import           Yesod
import           Yesod.Auth
import           Yesod.Core.Types (Logger)
import           Yesod.Default.Config
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static

import qualified HSync.Server.Settings                as Settings
import qualified Yesod.JoinPath as Y


--------------------------------------------------------------------------------

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data HSyncServerImplementation = HSyncServerImplementation
    { settings         :: AppConfig DefaultEnv Extra
    , getStatic'       :: Static -- ^ Settings for static file serving.
    , httpManager      :: Manager
    , appLogger        :: Logger
    , notificationChan :: TChan Notification
    , acidSync         :: AcidSync
    }


type instance Implementation HSyncServer = HSyncServerImplementation

instance IsHSyncServerImplementation HSyncServerImplementation where
  getStaticSubSite = getStatic'


-- Set up i18n messages. See the message folder.
mkMessage "HSyncServer" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype HSyncServerRouteRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route HSyncServer = HSyncServerRoute
-- * Creates the value resourcesHSyncServer which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the HSyncServerRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
-- mkYesodData "HSyncServer" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT HSyncServer IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod HSyncServer where
    approot = ApprootMaster $ appRoot . settings . implementation

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout mainContent = do
        -- master <- getImplementation
        mmsg   <- getMessage
        muser  <- maybeAuthId

        let loginR'     = AuthR loginR
            userMenu    = maybe $(widgetFile "loginForm")
                                (\user -> $(widgetFile "userLoggedIn"))
                                muser
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
                $(combineStylesheets 'StaticR [ css_bootstrap_css --  css_normalize_css
                                              ])
                $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y root) $ renderRoute s
      where
        root = Settings.staticRoot . settings . implementation $ y
    urlRenderOverride _ _ = Nothing

    -- Routes not requiring authentication.
    isAuthorized MyLoginR  _ = return Authorized
    isAuthorized FaviconR  _ = return Authorized
    isAuthorized RobotsR   _ = return Authorized

    isAuthorized HomeR     _ = return Authorized

    -- Listening for file changes requires listen access
    isAuthorized (ListenNowR p) _ = requireListen p
    isAuthorized (ListenR _ p)  _ = requireListen p

    -- Reading files, deltas, or patches requires Read Access
    isAuthorized (TreeR p)      _ = requireRead p
    isAuthorized (FileR p)      _ = requireRead p
    isAuthorized (DeltaR p)     _ = requireRead p
    isAuthorized (SignatureR p) _ = requireRead p

    -- Writing stuff requires write access
    isAuthorized (DeleteR _ p)  _ = requireWrite p
    isAuthorized (PatchR _ p)   _ = requireWrite p
    isAuthorized (PutFileR _ p) _ = requireWrite p
    isAuthorized (PutDirR _ p)  _ = requireWrite p

    -- Viewing the tree and the state requires read access
    isAuthorized (ViewTreeR p)  _ = requireRead p
    isAuthorized (ViewStateR p) _ = requireRead p

    -- The auth pages are normally authorized, For the register page
    -- it explicitly depends on whether or not we allow registration
    isAuthorized (AuthR r) _
      | r == registerR       = (\e -> if extraAllowRegister e
                                        then Authorized
                                        else Unauthorized "Forbidden."
                               ) <$> getExtra
    isAuthorized (AuthR _) _ = return Authorized



    -- For anything else: Forbid it for now
    isAuthorized _              _ = return $ Unauthorized "Forbidden."


    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    joinPath = Y.joinPath

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger . implementation

    maximumContentLength _ (Just r)
                     | postsBigFile r = Nothing
                   where
                     postsBigFile (PatchR _ _)   = True
                     postsBigFile (PutFileR _ _) = True
                     postsBigFile _              = False
    maximumContentLength _ _          = Just $ 2 * 1024 * 1024 -- 2 megabytes


-- How to access the stuff that we store using acid state
instance HasAcidState (HandlerT HSyncServer IO) FSState where
  getAcidState = fsState <$> getAcidSync

instance HasAcidState (HandlerT HSyncServer IO) UserIndex where
  getAcidState = users <$> getAcidSync

-- How to authenticate
instance YesodAuth HSyncServer where
    type AuthId HSyncServer = User

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = case userIdent . credsIdent $ creds of
                        Left _   -> return Nothing
                        Right ui -> (queryAcid $ LookupUser ui)

    maybeAuthId = do
      m <- lookupSession credsKey
      return $ m >>= fromPathPiece

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [localAuth]

    authHttpManager = httpManager . implementation

-- credsKey :: Text
-- credsKey = "_ID"

instance YesodLocalAuth HSyncServer where
  onRegister = createFilesDir

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage HSyncServer FormMessage where
    renderMessage _ _ = defaultFormMessage


getImplementation :: Handler HSyncServerImplementation
getImplementation = implementation <$> getYesod

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = appExtra . settings <$> getImplementation

getFilesDir :: Handler FilePath
getFilesDir = extraFilesDir <$> getExtra

getAcidSync :: Handler AcidSync
getAcidSync = acidSync <$> getImplementation

asLocalPath   :: Path -> Handler FilePath
asLocalPath p = flip toFilePath p . pack <$> getFilesDir
                -- TODO: The packing and unpacking is silly




-- | Create the directory for this user, and update the FSState
createFilesDir   :: User -> Handler ()
createFilesDir u = let n       = unpack . unUI . userId $ u
                       a </> b = a <> "/" <> b
                    in
                    do
                      fd <- getFilesDir
                      let fp = fd </> n
                      liftIO $ createDirectory fp
                      newUserFSState fp
                        >>= updateAcid . NewUserDirectory (userId u)



requireRead             :: Path -> Handler AuthResult
requireRead (Path ui _) = maybeAuthId >>= maybe (return AuthenticationRequired) checkUI
  where
    checkUI u
      | userId u == ui = return Authorized
      | otherwise      = return $ Unauthorized "No Access"

requireListen :: Path -> Handler AuthResult
requireListen = requireRead

-- | For routes/paths that require write access
requireWrite :: Path -> Handler AuthResult
requireWrite = requireRead
--  For now require the same thing as reading
