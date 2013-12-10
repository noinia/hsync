{-# Language  RankNTypes #-}
{-# Language  FlexibleContexts #-}
module HSync.Client.RemoteEvents where

import Control.Concurrent(forkIO)
import Control.Failure


import Data.Conduit
import Data.Conduit.Internal(ResumableSource(..))

import HSync.Client.Import

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.Sync(Sync)

import HSync.Common.AtomicIO
import HSync.Common.DateTime(DateTime)
-- import HSync.Common.FileIdent
import HSync.Common.Notification

import Network.HTTP.Conduit( HttpException(..) )



--------------------------------------------------------------------------------
-- | Handle Notifications


handleNotification                        :: ( MonadResource m, Failure HttpException m
                                             , MonadBaseControl IO m
--                                             , MonadBaseControl IO (ActionT (ResourceT m)
                                               ) =>
                                             Notification -> ActionT m ()
handleNotification n@(Notification e ci t) = do
                                               sync <- getSync
                                               fp   <- toLocalPath p
                                               liftIO $ handleAtomic sync fp
  where
    p                    = affectedFromPath e
    -- handleAtomic :: Sync -> FilePath -> IO ()
    handleAtomic sync fp = atomicallyWriteIO fp . runResourceT $ runActionT act sync
    act                  = protect (noConflict e t)
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
-- the future), we run a handler that updates our local file system.
syncDownstream     :: ( MonadResource m, Failure HttpException m
                      , MonadBaseControl IO m) =>
                      DateTime -> Path -> ActionT m ()
syncDownstream dt p = do
                        sync            <- getSync
                        changesSource'  <- changes dt p
                        liftIO $ print "syncing downstream!"
                        -- lift $ changesSource' $$+- printSink
                        let changesSource = transPipe' lift changesSource'
                        changesSource $$+- notificationSink

-- | The function transPipe for ResumableSources. Note that we use the supplied
-- lifting function on both the source as the finalizer.
transPipe' ::  Monad m => (forall a. m a -> n a) ->
               ResumableSource m o -> ResumableSource n o
transPipe' lft (ResumableSource s final) = ResumableSource (transPipe lft s) (lft final)


printSink :: (Show a, MonadIO m) => Sink a m ()
printSink = awaitForever (liftIO . print)


-- | A sink that, for each incoming notification runs the approprieate action
-- to handle it.
notificationSink      :: (MonadResource m, Failure HttpException m
                         , MonadBaseControl IO m) =>
                         Sink Notification (ActionT m) ()
-- notificationSink = awaitForever (lift .handleNotification)
notificationSink = awaitForever handle
  where
    handle n = do
                 liftIO $ print n
                 lift $ handleNotification n


-- where
--   handleNotification' n = handleNotification


  --     liftIO . forkIO $ runResourceT $
  --                             runActionT (handleNotification n) sync
