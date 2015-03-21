{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.Server.Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where


import Control.Concurrent(forkIO, ThreadId)
import Control.Concurrent.STM.TChan

import Data.Default (def)

import HSync.Server

import HSync.Server.Import
import HSync.Server.AcidSync(AcidSync, withAcidSync)
import HSync.Server.Notifications(storeNotifications, printNotifications)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import HSync.Server.Handler.Home
import HSync.Server.Handler.Auth
import HSync.Server.Handler.FileActions
import HSync.Server.Handler.ViewTree

import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )

import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)

import Yesod.Default.Main(defaultDevelApp)
import Yesod.Default.Config
import Yesod.Default.Handlers
import Yesod.Core.Types (loggerSet, Logger (Logger))

import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

--------------------------------------------------------------------------------


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.

mkYesodDispatch "HSyncServer" resourcesHSyncServer

-- instance YesodDispatch HSyncServer where
--   --  yesodDispatch :: YesodRunnerEnv HSyncServer -> Application
--      -- Application ~ Request -> IO Response
--   yesodDispatch yre request = return undefined



-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication           :: AcidSync -> AppConfig DefaultEnv Extra -> IO Application
makeApplication acid conf = do
    foundation' <- makeFoundation acid conf
    let foundation = implementation foundation'

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    _ <- startNotificationLoggers foundation

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation' -- vs toWaiApp
    return $ logWare app



-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation           :: AcidSync -> AppConfig DefaultEnv Extra -> IO HSyncServer
makeFoundation acid conf = do
    manager     <- newManager conduitManagerSettings
    s           <- staticSite
    nots        <- newBroadcastTChanIO
    loggerSet'  <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger     = Yesod.Core.Types.Logger loggerSet' getter
        foundation = HSyncServerImplementation conf s manager logger nots acid

    return $ HSyncServer foundation


-- -- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
  config <- loader
  withAcidSync (appExtra config) $ \acid ->
    defaultDevelApp (return config) (makeApplication acid)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
             { csParseExtra = parseExtra
             }


startNotificationLoggers    :: HSyncServerImplementation -> IO [ThreadId]
startNotificationLoggers hss = mapM forkIO [ storeNotifications hss
                                           , printNotifications hss
                                           ]
