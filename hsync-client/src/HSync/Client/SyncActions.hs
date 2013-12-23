{-# LANGUAGE FlexibleContexts #-}
module HSync.Client.SyncActions where

import Control.Applicative((<$>))
import Control.Exception.Lifted(bracket)

import Control.Monad.IO.Class (liftIO, MonadIO )

import Data.Acid(openLocalState)
import Data.Acid.Local(createCheckpointAndClose)

import Data.Default
import Data.Conduit(ResourceT, runResourceT, MonadThrow, MonadUnsafeIO, MonadBaseControl)
import Data.Either

import HSync.Client.Actions(login, putFile)
import HSync.Client.ActionT(Action, runActionT, getSync)
import HSync.Client.AcidSync(AcidSync(..))
import HSync.Client.Sync
import HSync.Client.RemoteEvents


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
                    esync <- liftIO $ readConfig fp
                    bracket (openLocalState def)
                            (createCheckpointAndClose)
                            (\acid -> withManager $ \mgr -> case esync of
                                Left errMsg -> liftIO $ print errMsg
                                Right sync' -> let sync = sync' { httpManager = mgr }
                                               in runActionT act sync (AcidSync acid)
                            )

listenMain    :: FilePath -> IO ()
listenMain fp = withSync fp $ do
                                u <- user <$> getSync
                                now <- liftIO $ currentTime
                                liftIO $ print "ok!"
                                login
                                syncDownstream now $ Path u []


-- -- | The main method for the sync with config fp
syncMain    :: FilePath -> IO ()
syncMain fp = listenMain fp


putMain fp = withSync fp $ do
  sync <- getSync
  fs' <- liftIO $ getDirectoryContents "/Users/frank/tmp/synced/tls"
  let fs = map ("/Users/frank/tmp/synced/tls/" ++) . filter (not . isPrefixOf ".") $ fs'
  login
  liftIO $ mapM_ print fs
  mapM_ putFile fs

downloadMain fp = withSync fp $ do
  sync <- getSync
  login
  cloneDownstream $ Path (user sync) []
