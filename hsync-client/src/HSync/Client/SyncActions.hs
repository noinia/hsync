{-# LANGUAGE FlexibleContexts #-}
module HSync.Client.SyncActions where

import Control.Applicative((<$>))

import Control.Monad.IO.Class (liftIO, MonadIO )


import Data.Conduit(ResourceT, runResourceT, MonadThrow, MonadUnsafeIO, MonadBaseControl)
import Data.Either

import HSync.Client.ActionT(ActionT, runActionT, getSync)
import HSync.Client.Sync
import HSync.Client.RemoteEvents


import HSync.Common.DateTime
import HSync.Common.Types

import Network.HTTP.Conduit( withManager)

--------------------------------------------------------------------------------

-- | Given a path to a sync config file, Loads the config file, and starts a
-- new connection manager, that we associate with the sync. Then we run the
-- given action.
withSync :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadUnsafeIO m) =>
            FilePath -> ActionT (ResourceT m) () -> m ()
withSync fp act = withManager $ \mgr -> do
                              esync <- liftIO $ readConfig fp
                              case esync of
                                Left errMsg -> liftIO $ print errMsg
                                Right sync' -> let sync = sync' { httpManager = mgr }
                                               in runActionT act sync

listenMain   :: FilePath -> IO ()
listenMain fp = withSync fp $ do
                                u <- user <$> getSync
                                now <- liftIO $ currentTime
                                syncDownstream now $ Path u []


-- -- | The main method for the sync with config fp
syncMain    :: FilePath -> IO ()
syncMain fp = withSync fp $ return ()
