{-# Language  FlexibleContexts #-}
module HSync.Client.RemoteEvents where

import Control.Concurrent(forkIO)
import Control.Failure


import Data.Conduit

import HSync.Client.Import


import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.Sync

-- import HSync.Common.Types
import HSync.Common.DateTime(DateTime)
-- import HSync.Common.FileIdent
import HSync.Common.Notification

import Network.HTTP.Conduit( HttpException(..) )



--------------------------------------------------------------------------------
-- | Handle Notifications

handleNotification                        :: ( MonadResource m, Failure HttpException m
                                             , MonadBaseControl IO m) =>
                                             Notification -> ActionT m ()
handleNotification n@(Notification e ci t) = protect (noConflict e t)
                                                     (handleEvent e)
                                                     (handleIncomingConflict n)



handleEvent                         :: (Failure HttpException m,
                                        MonadBaseControl IO m, MonadResource m) =>
                                       EventKind -> ActionT m ()
handleEvent (FileAdded p)           = getFile p
handleEvent (FileRemoved p fi)      = deleteFileLocally p fi
handleEvent (FileUpdated p fi)      = getUpdate p fi
handleEvent (FileMoved f fi t)      = error "handleEvent: moveFile not implemented yet"

handleEvent (DirectoryAdded p)      = createDirectoryLocally p
handleEvent (DirectoryRemoved p fi) = return ()
handleEvent (DirectoryUpdated p)    = return () -- TODO: not sure what to do here?
handleEvent (DirectoryMoved f t)    = error "handleEvent: moveDir not implemented yet"


--------------------------------------------------------------------------------
-- | Conflict Checking

noConflict e t = return True --TODO


conflictsLocal         :: Monad m =>
                          Path -> Maybe FileIdent -> DateTime -> ActionT m Bool
conflictsLocal p mfi t = return False -- TODO!!




--------------------------------------------------------------------------------
-- | Conflict Handling

handleIncomingConflict _ = return ()

--------------------------------------------------------------------------------
-- | Syncing

-- | Start syncing the files in path p. To do this, get all notifications
-- starting at dt. For each notification that we receive (and will receive in
-- the future), we fork a thread and run a handler that updates our local file
-- system.
syncDownstream     :: ( MonadResource m, Failure HttpException m
                      , MonadBaseControl IO m) =>
                      DateTime -> Path -> ActionT m ()
syncDownstream dt p = do
                        sync <- getSync
                        chs  <- changes dt p
                        lift $ chs $$+- notificationSink sync

-- | A sink that, for each incoming notification, forks a new thread, and in
-- this thread runs an action to handle the incoming notification.
notificationSink      :: MonadIO m => Sync -> Sink Notification m ()
notificationSink sync = awaitForever handleNotification'
  where
    handleNotification' n = liftIO . forkIO $ runResourceT $
                              runActionT (handleNotification n) sync
