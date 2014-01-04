{-# Language  RankNTypes #-}
{-# Language  FlexibleContexts #-}
module HSync.Client.RemoteEvents where

import Control.Monad.IO.Class(MonadIO(..))

import Control.Monad.Trans.Class(lift)
import Control.Concurrent(forkIO)
import Control.Failure


import Data.Conduit
import Data.Conduit.Internal(ResumableSource(..))

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.Sync(Sync)
import HSync.Client.AcidActions

import HSync.Common.Import
import HSync.Common.AtomicIO
import HSync.Common.DateTime(DateTime)
import HSync.Common.Notification

import HSync.Common.FSTree
import HSync.Common.Types

import Network.HTTP.Conduit( HttpException(..) )

import System.Directory( doesDirectoryExist)

import qualified HSync.Common.FileIdent as FI


--------------------------------------------------------------------------------
-- | Handle Notifications


handleNotification                         :: Notification -> Action ()
handleNotification n@(Notification e ci t) = do
                                               sync   <- getSync
                                               fp     <- toLocalPath p
                                               yState <- getYesodClientState
                                               acid   <- getAcidSync
                                               liftIO $ handleAtomic acid yState sync fp
  where
    p = affectedPath e
    -- handleAtomic :: AcidSync -> Sync -> FilePath -> IO ()
    handleAtomic acid yState sync fp = atomicallyWriteIO fp . runResourceT $
                                         runActionTWithClientState yState sync acid act
    act = protect (noConflict e t)
                  (handleEvent e)
                  (handleIncomingConflict n)



handleEvent                                      :: Event -> Action ()
handleEvent (Event FileAdded p _)                = getFile p
handleEvent (Event FileRemoved p (Just fi))      = deleteFileLocally p fi
handleEvent (Event FileUpdated p (Just fi))      = getUpdate p fi

handleEvent (Event DirectoryAdded p _)           = createDirectoryLocally p
handleEvent (Event DirectoryRemoved p (Just fi)) = return () -- TODO
handleEvent e                                    = let es = show e in error $
                                     "handleEvent: inconsistent event: " ++ es

--------------------------------------------------------------------------------
-- | Conflict Checking

noConflict e t = return True --TODO


conflictsLocal         :: Path -> Maybe FI.FileIdent -> DateTime -> Action Bool
conflictsLocal p mfi t = return False -- TODO!!




--------------------------------------------------------------------------------
-- | Conflict Handling

handleIncomingConflict _ = return ()

--------------------------------------------------------------------------------
-- | Cloning


-- | Download/copy/clone the tree indicated by path. We assume that the local
-- directory corresponding to path is empty. This action will replace the
-- remotetree state, so it is unsafe to run other actions in parallel with
-- this!
cloneDownstream   :: Path -> Action ()
cloneDownstream p = do
                      mt <- getRemoteTree p
                      downloadTree mt
                      updateTreeState' (const mt) -- store that we have fetched
                                                  -- a new remote tree.
                      serverTreeState >>= liftIO . print
  where
    downloadTree Nothing      = error "cloneDownStream: no tree"
                                  -- TODO: fix the error stuff
    downloadTree (Just (F _)) = getFile p
    downloadTree (Just (D d)) = cloneDirectoryDownstream p d



cloneDirectoryDownstream       :: Path -> Directory fl dl -> Action ()
cloneDirectoryDownstream p dir = protect dirExists
                                         ifAct
                                         elseAct
  where
    (sds,fs)      = (subDirectories dir, files dir)
    dirExists     = toLocalPath p >>= liftIO . doesDirectoryExist
    ifAct         = downloadDirs >> downloadFiles
    downloadDirs  = mapM_ (\d -> let p' = append . dirName $ d in
                                cloneDirectoryDownstream p' d) sds
    downloadFiles = mapM_ (getFile .  append . fileName) fs
    elseAct       = createDirectoryLocally p >> ifAct
    append n      = p { subPath = subPath p ++ [n]}


--------------------------------------------------------------------------------
-- | Syncing

-- | Start syncing the files in path p. To do this, get all notifications
-- starting at dt. For each notification that we receive (and will receive in
-- the future), we run a handler that updates our local file system.
syncDownstream      :: DateTime -> Path -> Action ()
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
notificationSink :: Sink Notification Action ()
-- notificationSink = awaitForever (lift .handleNotification)
notificationSink = awaitForever handle
  where
    handle n = do
                 liftIO $ print n
                 lift $ handleNotification n
