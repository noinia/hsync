module HSync.Server.Foundation where

import Prelude

import Control.Applicative((<$>))

import Control.Concurrent.STM.TChan

import HSync.Common.DateTime(DateTime)
import HSync.Common.Types
import HSync.Common.FileIdent(FileIdent)
import HSync.Common.Notification

import Data.Default
import Data.Text(pack)

import Database.Persist.Sql (SqlPersistT)

import HSync.Server.Settings (widgetFile, Extra (..))
import HSync.Server.Settings.StaticFiles
import HSync.Server.Settings.Development (development)
import HSync.Server.Model
import HSync.Server.AcidSync

import Network.HTTP.Conduit (Manager)

import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)

import Yesod
import Yesod.Auth
import Yesod.Core.Types(Logger)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static


import qualified HSync.Server.Settings as Settings
import qualified Database.Persist

--------------------------------------------------------------------------------

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data HSyncServer = HSyncServer
    { settings         :: AppConfig DefaultEnv Extra
    , getStatic        :: Static -- ^ Settings for static file serving.
    , connPool         :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager      :: Manager
    , persistConfig    :: Settings.PersistConf
    , appLogger        :: Logger
    , notificationChan :: TChan Notification
    , acidSync         :: AcidSync
    }


instance Default HSyncServer where
    -- | only use this def instance to create a value of the type HSyncServer
    def = HSyncServer { settings         = undefined
                      , getStatic        = undefined
                      , connPool         = undefined
                      , httpManager      = undefined
                      , persistConfig    = undefined
                      , appLogger        = undefined
                      , notificationChan = undefined
                      , acidSync         = undefined
                      }

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
mkYesodData "HSyncServer" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT HSyncServer IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod HSyncServer where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

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

    makeLogger = return . appLogger

    maximumContentLength _ (Just r)
                     | postsBigFile r = Nothing
                   where
                     postsBigFile (PatchR _ _)   = True
                     postsBigFile (PutFileR _ _) = True
                     postsBigFile _              = False
    maximumContentLength _ _          = Just $ 2 * 1024 * 1024 -- 2 megabytes


-- How to run database actions.
instance YesodPersist HSyncServer where
    type YesodPersistBackend HSyncServer = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner HSyncServer where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth HSyncServer where
    type AuthId HSyncServer = UserIdent

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        return $ case x of
            Just (Entity _ (User uid _)) -> Just uid
            Nothing                      -> Nothing

    maybeAuthId = do
      m <- lookupSession credsKey
      return $ m >>= fromPathPiece

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = []

    authHttpManager = httpManager

-- credsKey :: Text
-- credsKey = "_ID"

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage HSyncServer FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = appExtra . settings <$> getYesod

getFilesDir :: Handler FilePath
getFilesDir = extraFilesDir <$> getExtra

getAcidSync :: Handler AcidSync
getAcidSync = acidSync <$> getYesod

asLocalPath   :: Path -> Handler FilePath
asLocalPath p = flip toFilePath p . pack <$> getFilesDir
                -- TODO: The packing and unpacking is silly
