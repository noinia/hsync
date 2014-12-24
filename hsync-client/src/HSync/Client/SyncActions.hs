{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module HSync.Client.SyncActions where

import Prelude hiding (FilePath)

import Control.Applicative((<$>),(<*>))
import Control.Concurrent(forkIO, killThread, ThreadId)
import Control.Exception.Lifted(bracket)

import Control.Monad(when)
import Control.Monad.IO.Class (liftIO, MonadIO )
import Control.Monad.Catch(MonadThrow(..))
import Control.Monad.Trans.Resource(ResourceT, runResourceT)
import Control.Monad.Trans.Control(MonadBaseControl)

import Data.Aeson(ToJSON(..), FromJSON(..))
import Data.Aeson.TH(deriveJSON, defaultOptions)
import Data.Acid(openLocalStateFrom)
import Data.Acid.Local(createCheckpointAndClose)

import Data.Data(Data,Typeable)
import Data.Default
import Data.Either
import Data.Maybe(isNothing, fromMaybe)

import Filesystem.Path.CurrentOS(FilePath, encodeString)

import HSync.Client.AcidActions(serverTreeState, lastChange)
import HSync.Client.Actions(login, forcePutFile)
import HSync.Client.ActionT(Action, runActionT, getSync, cloneInIO)
import HSync.Client.AcidSync(AcidSync(..))
import HSync.Client.Logger
import HSync.Client.Sync
import HSync.Client.RemoteEvents
import HSync.Client.LocalEvents
import HSync.Client.TemporaryIgnored





import HSync.Common.DateTime
import HSync.Common.Types

import Network.HTTP.Conduit( withManager)



import System.Directory
import Data.List(isPrefixOf)

--------------------------------------------------------------------------------

-- | Data type that defines if we should synchronize only downstream, only
-- upstream, or both.
data SyncMode = DownStream | UpStream | BiDirectional
                   deriving (Show,Read,Eq,Data,Typeable)
$(deriveJSON defaultOptions ''SyncMode)

shouldListenLocal x = x == UpStream || x == BiDirectional

shouldListenRemote x = x == DownStream || x == BiDirectional

--------------------------------------------------------------------------------

-- | Given a path to a sync config file, Loads the config file, and starts a
-- new connection manager, that we associate with the sync. Then we run the
-- given action.
withSync        :: FilePath -> Action () -> IO ()
withSync fp act = do
                    esync    <- liftIO $ readConfig fp
                    tIgnores <- initializeTemporaryIgnored
                    case esync of
                      Left errMsg -> liftIO $ print errMsg
                      Right sync' -> bracket (openLocalStateFrom (statePath' sync') def)
                                             (createCheckpointAndClose)
                                             (\acid -> withManager $ \mgr ->
                                               let sync = sync' { httpManager = mgr }
                                                   s    = tIgnores
                                                   r    = AcidSync acid
                                               in runActionT act sync s r
                                             )
  where
    statePath' = encodeString . statePath




launchSyncFrom       :: FilePath -> SyncMode -> IO ()
launchSyncFrom fp sm = withSync fp $ do
    sync <- getSync
    _    <- login
    mt   <- serverTreeState
    when (isNothing mt) firstRun
    let stop = stopOnEnter
    mtid <- if shouldListenRemote sm then Just <$> listenRemote sync stop
                                     else return Nothing
    _    <- if shouldListenLocal  sm then          listenLocal  sync stop
                                     else return ()
    liftIO $ maybe (return ()) killThread mtid



  --   dt   <- lastChange'
  --   let rbp = Path (user sync) (remoteBaseDir sync)
  --   infoM "SyncActions.syncMain" "Start listening for remote changes"
  --   -- Start a new thread in which we listen for new remote changes
  --   tid  <- (cloneInIO $ syncDownstream dt rbp) >>= liftIO . forkIO
  --   -- We will start listening for local changes
  --   syncUpstreamUntil stop rbp
  --   -- we have stopped listening for local changes so also stop listening for
  --   -- remote changes
  --   liftIO $ killThread tid
  -- where
  --   stop = stopOnEnter


-- | Given a sync, and an 'stop'-action, listen for local changes until
-- 'stop'. (i.e. we keep listening until the stop action finishes.)
listenLocal           :: Sync -> IO () -> Action ()
listenLocal sync stop =
    syncUpstreamUntil stop (remoteBasePath sync)

-- | Given a sync, and a stop-action, start a new thread in which we listen for
-- new remote changes until the stop action finishes.
listenRemote      :: Sync -> IO () -> Action ThreadId
listenRemote sync stop = do
    dt   <- lastChange'
    infoM "SyncActions.listenRemote" "Start listening for remote changes"
    ioA <- cloneInIO $ syncDownstream dt (remoteBasePath sync)
    liftIO  . forkIO $ ioA
  where
    lastChange' = fromMaybe <$> currentTime
                            <*> lastChange














stopOnEnter :: IO ()
stopOnEnter = putStrLn "Press enter to stop" >> getLine >> return ()


-- | Things to do on the first run
firstRun :: Action ()
firstRun = do
             sync <- getSync
             cloneDownstream $ Path (user sync) (remoteBaseDir sync)








-- downloadMain fp = withSync fp $ do
--   sync <- getSync
--   login
--   cloneDownstream $ Path (user sync) (remoteBaseDir sync)


-- uploadMain fp = withSync fp $ do
--                                 u <- user <$> getSync
--                                 login
--                                 syncUpstreamUntil stopOnEnter $ Path u [""]


-- showState fp = withSync fp $ do
--                                Just t <- serverTreeState
--                                liftIO . putStr $ show t


-- listenMain    :: FilePath -> IO ()
-- listenMain fp = withSync fp $ do
--                                 u <- user <$> getSync
--                                 now <- liftIO $ currentTime
--                                 liftIO $ print "ok!"
--                                 login
--                                 syncDownstream now $ Path u []

-- -- | The main method for the sync with config fp
-- syncMain    :: FilePath -> IO ()
-- syncMain fp = withSync fp $ do
--     sync <- getSync
--     _    <- login
--     mt   <- serverTreeState
--     when (isNothing mt) firstRun
--     dt   <- lastChange'
--     let rbp = Path (user sync) (remoteBaseDir sync)
--     infoM "SyncActions.syncMain" "Start listening for remote changes"
--     -- Start a new thread in which we listen for new remote changes
--     tid  <- (cloneInIO $ syncDownstream dt rbp) >>= liftIO . forkIO
--     -- We will start listening for local changes
--     syncUpstreamUntil stop rbp
--     -- we have stopped listening for local changes so also stop listening for
--     -- remote changes
--     liftIO $ killThread tid
--   where
--     stop = stopOnEnter

--     lastChange' = fromMaybe <$> currentTime
--                             <*> lastChange
