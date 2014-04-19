{-# LANGUAGE FlexibleContexts #-}
module HSync.Client.SyncActions where

import Control.Applicative((<$>))
import Control.Concurrent(forkIO, killThread)
import Control.Exception.Lifted(bracket)

import Control.Monad(when)
import Control.Monad.IO.Class (liftIO, MonadIO )
import Control.Monad.Catch(MonadThrow(..))
import Control.Monad.Trans.Resource(ResourceT, runResourceT)
import Control.Monad.Trans.Control(MonadBaseControl)

import Data.Acid(openLocalStateFrom)
import Data.Acid.Local(createCheckpointAndClose)

import Data.Default
import Data.Either
import Data.Maybe(isNothing)


import HSync.Client.Actions(login, forcePutFile)
import HSync.Client.ActionT(Action, runActionT, getSync, cloneInIO)
import HSync.Client.AcidSync(AcidSync(..))
import HSync.Client.Logger
import HSync.Client.Sync
import HSync.Client.RemoteEvents
import HSync.Client.LocalEvents
import HSync.Client.TemporaryIgnored


-- Remove when done debugging
import HSync.Client.AcidActions(serverTreeState)
import HSync.Common.FSTree.Basic(prettyPrintTree)
--


import HSync.Common.DateTime
import HSync.Common.Types

import Network.HTTP.Conduit( withManager)



import System.Directory
import Data.List(isPrefixOf)

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
                      Right sync' -> bracket (openLocalStateFrom (statePath sync') def)
                                             (createCheckpointAndClose)
                                             (\acid -> withManager $ \mgr ->
                                               let sync = sync' { httpManager = mgr }
                                                   s    = tIgnores
                                                   r    = AcidSync acid
                                               in runActionT act sync s r
                                             )


listenMain    :: FilePath -> IO ()
listenMain fp = withSync fp $ do
                                u <- user <$> getSync
                                now <- liftIO $ currentTime
                                liftIO $ print "ok!"
                                login
                                syncDownstream now $ Path u []



-- | The main method for the sync with config fp
syncMain    :: FilePath -> IO ()
syncMain fp = withSync fp $ do
    sync <- getSync
    _    <- login
    mt   <- serverTreeState
    when (isNothing mt) firstRun
    dt   <- currentTime
    let rbp = Path (user sync) (remoteBaseDir sync)
    infoM "SyncActions.syncMain" "Start listening for remote changes"
    -- Start a new thread in which we listen for new remote changes
    tid  <- (cloneInIO $ syncDownstream dt rbp) >>= liftIO . forkIO
    -- We will start listening for local changes
    syncUpstreamUntil stop rbp
    -- we have stopped listening for local changes so also stop listening for
    -- remote changes
    liftIO $ killThread tid
  where
    stop = stopOnEnter

stopOnEnter :: IO ()
stopOnEnter = putStrLn "Press enter to stop" >> getLine >> return ()


-- | Things to do on the first run
firstRun :: Action ()
firstRun = do
             sync <- getSync
             cloneDownstream $ Path (user sync) (remoteBaseDir sync)









putMain fp = withSync fp $ do
  sync <- getSync
  fs' <- liftIO $ getDirectoryContents "/Users/frank/tmp/synced/tls"
  let fs = map ("/Users/frank/tmp/synced/tls/" ++) . filter (not . isPrefixOf ".") $ fs'
  login
  liftIO $ mapM_ print fs
  mapM_ forcePutFile fs

downloadMain fp = withSync fp $ do
  sync <- getSync
  login
  cloneDownstream $ Path (user sync) (remoteBaseDir sync)


uploadMain fp = withSync fp $ do
                                u <- user <$> getSync
                                login
                                syncUpstreamUntil stopOnEnter $ Path u [""]


showState fp = withSync fp $ do
                               Just t <- serverTreeState
                               liftIO . putStr $ prettyPrintTree t
