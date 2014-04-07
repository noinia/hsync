{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.Server.Application
    ( makeApplication
    -- , getApplicationDev
    , makeFoundation
    ) where


import Control.Concurrent(forkIO, ThreadId)
import Control.Concurrent.STM.TChan

import Data.Default (def)

import HSync.Server.Import
import HSync.Server.AcidSync(AcidSync)
import HSync.Server.Notifications(storeNotifications)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import HSync.Server.Handler.Home
import HSync.Server.Handler.Auth
import HSync.Server.Handler.FileActions

import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )

import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)

import Yesod.Default.Config
import Yesod.Default.Handlers
import Yesod.Core.Types (loggerSet, Logger (Logger))

import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

--------------------------------------------------------------------------------


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "HSyncServer" resourcesHSyncServer

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication           :: AcidSync -> AppConfig DefaultEnv Extra -> IO Application
makeApplication acid conf = do
    foundation <- makeFoundation acid conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    _ <- startNotificationLogger foundation

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
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
        foundation = HSyncServer conf s manager logger nots acid

    return foundation


-- -- for yesod devel
-- getApplicationDev :: IO (Int, Application)
-- getApplicationDev =
--     defaultDevelApp loader makeApplication
--   where
--     loader = Yesod.Default.Config.loadConfig (configSettings Development)
--         { csParseExtra = parseExtra
--         }



startNotificationLogger :: HSyncServer -> IO ThreadId
startNotificationLogger hss = forkIO start
  where
    start :: IO ()
    start = storeNotifications hss
