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
import Control.Monad.Logger (runLoggingT)

import Data.Conduit(runResourceT)

import Database.Persist.Sql (runMigration)


import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Conduit (newManager, def)

import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers



import HSync.Server.Handler.Home
import HSync.Server.Handler.Auth
import HSync.Server.Handler.FileActions


import HSync.Server.NotificationLog(logNotificationsToFile)

import qualified Database.Persist

--------------------------------------------------------------------------------



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
        , destination = Logger $ appLogger foundation
        }


    -- Start the Notification logging component
    -- startNotificationLogger foundation

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO HSyncServer
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    logger <- mkLogger True stdout
    nots <- newBroadcastTChanIO
    let foundation = HSyncServer conf s p manager dbconf logger nots

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation


startNotificationLogger     :: HSyncServer -> IO ThreadId
startNotificationLogger hss = forkIO start
    where
      start :: IO ()
      start = runResourceT $ notifications' hss >>= logNotificationsToFile dir
      dir   = extraNotificationLogDir . appExtra . settings $ hss


-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
