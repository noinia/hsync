{-# Language  RankNTypes #-}
{-# Language  FlexibleContexts #-}
module HSync.Client.RemoteEvents where

import Control.Applicative((<$>))
import Control.Concurrent(forkIO)

import Control.Monad(when)
import Control.Monad.IO.Class(MonadIO(..))
import Control.Monad.Trans.Class(lift)

import Data.Maybe(isNothing)
import Data.Monoid(mconcat)

import Data.Conduit
import Data.Conduit.Internal(ResumableSource(..))

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.Sync(Sync, clientIdent)
import HSync.Client.AcidActions

import HSync.Common.Import
import HSync.Common.AtomicIO
import HSync.Common.DateTime(DateTime(..), showDateTime, modificationTime)
import HSync.Common.Notification

import HSync.Common.FSTree
import HSync.Common.Types

import Network.HTTP.Conduit( HttpException(..) )

import System.Directory(doesDirectoryExist, renameFile, renameDirectory)
import System.FilePath(takeBaseName, replaceBaseName)

import qualified Data.Text              as T
import qualified HSync.Common.FileIdent as FI


--------------------------------------------------------------------------------
-- | Handle Notifications


handleNotification                         :: Notification -> Action ()
handleNotification n@(Notification e ci t) = do
                                               fp  <- toLocalPath p
                                               ioA <- cloneInIO act
                                               liftIO $
                                                 atomicallyWriteIO fp ioA
  where
    p   = affectedPath e
    act = protect (noConflict e)
                  (handleEvent e)
                  (handleConflict p t)

handleEvent                                         :: Event -> Action ()
handleEvent (Event FileAdded p FI.NonExistent)      = getFile p
handleEvent (Event FileRemoved p fi)                = deleteFileLocally p fi
handleEvent (Event FileUpdated p fi)                = getUpdate p fi

handleEvent (Event DirectoryAdded p FI.NonExistent) = createDirectoryLocally p
handleEvent (Event DirectoryRemoved p fi)           = return () -- TODO
handleEvent e                                       = let es = show e in
          error $  "handleEvent: inconsistent event: " ++ es

--------------------------------------------------------------------------------
-- | Conflict Checking

noConflict   :: Event -> Action Bool
noConflict e = let p   = affectedPath e
                   rfi = affectedFileIdent e
               in isNothing <$> (toLocalPath p >>= FI.checkFileIdent rfi)

--------------------------------------------------------------------------------
-- | Conflict Handling

handleConflict      :: Path
                    -> DateTime  -- Time when the file was changed at the server
                    -> Action ()
handleConflict p rt = do
  fp <- toLocalPath p
  -- Move the local file or directory if it exists and then just download the
  -- tree anew from the server
  (b,_) <- exists fp
  when b $ do
             ci <- clientIdent <$> getSync
             mt <- modificationTime fp
             liftIO $ renameFileOrDir fp (conflictedFp fp ci mt rt)
  cloneDownstream p


-- | Rename function that works both for files and directories
renameFileOrDir        :: FilePath -> FilePath -> IO ()
renameFileOrDir fp fp' = atomicallyIO fp $ do
                           b <- doesDirectoryExist fp
                           let rename = if b then renameDirectory else renameFile
                           rename fp fp'


-- | Construct the filename to use for conflicted copies
conflictedFp             :: FilePath -> ClientIdent -> DateTime -> DateTime -> FilePath
conflictedFp fp ci lt rt = replaceBaseName fp $ mconcat [ takeBaseName fp
                                                        , "_conflicted_copy_"
                                                        , T.unpack . unCI $ ci
                                                        , "_local_modified_"
                                                        , showDateTime . unDT $ lt
                                                        , "_remote_modified_"
                                                        , showDateTime . unDT $ rt
                                                        ]

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
                      replaceServerStateBy mt -- store that we have fetched
                                              -- - a new remote tree.
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
