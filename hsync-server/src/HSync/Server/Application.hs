{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.Server.Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where


import HSync.Server.Import
import qualified HSync.Server.Settings as Settings

import Control.Concurrent(forkIO, ThreadId)
import Control.Concurrent.STM.TChan

import Data.Conduit(runResourceT, ($$), (=$), Sink)
import Data.Conduit.Binary(sinkFile)

--import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Control.Monad.Logger (runLoggingT)
import qualified GHC.IO.FD
import System.Log.FastLogger (newLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import HSync.Server.Handler.Home
import HSync.Server.Handler.Auth
import HSync.Server.Handler.FileActions

-- import HSync.Server.NotificationLog(logNotificationsToFile)
-- import HSync.Server.NotificationLog(simpleNotificationFileSink)

import qualified Data.Conduit.List as L
import qualified Data.ByteString.Char8 as B


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "HSyncServer" resourcesHSyncServer

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

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
makeFoundation :: AppConfig DefaultEnv Extra -> IO HSyncServer
makeFoundation conf = do
    manager <- newManager conduitManagerSettings
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    nots <- newBroadcastTChanIO
    loggerSet' <- newLoggerSet defaultBufSize GHC.IO.FD.stdout
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = HSyncServer conf s p manager dbconf logger nots

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

startNotificationLogger     :: HSyncServer -> IO ThreadId
startNotificationLogger hss = forkIO start
    where
      start :: IO ()
      start = runResourceT $ notifications' hss >>= \s ->
                               s $$ simpleNotificationFileSink dir
      dir   = extraNotificationLogDir . appExtra . settings $ hss


-- | A Sink that simply logs all notifcations to the given file
simpleNotificationFileSink   :: (MonadResource m, MonadIO m) =>
                                FilePath -> Sink Notification m ()
simpleNotificationFileSink f = L.map printNotification =$ sinkFile f
  where
    printNotification = B.pack . (++"\n") . toLog
