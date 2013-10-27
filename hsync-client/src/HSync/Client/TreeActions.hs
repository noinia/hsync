{-# Language  FlexibleContexts #-}
{-# Language  OverloadedStrings #-}
module HSync.Client.TreeActions where

import Prelude

import Control.Failure

import Control.Monad.IO.Class (liftIO)

import Data.Either

import HSync.Client.FSStatus

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.Import

import HSync.Common.AtomicIO
import HSync.Common.FSTree

import Network.HTTP.Conduit( HttpException(..) )

import System.Directory(removeFile , createDirectory )

import qualified HSync.Common.FileIdent     as FI


--------------------------------------------------------------------------------

syncTree               :: ( MonadResource m, Failure HttpException m
                          , MonadIO m, MonadBaseControl IO m) =>
                          Path -> ActionT m ()
syncTree p@(Path _ sp) = do
  oldRemote <- subTree sp <$> remoteTree
  liftIO $ print "getting remote tree"
  newRemote <- getRemoteTree p
  liftIO $ print "reading local tree"
  newLocal  <- readFSTree =<< toLocalPath p
  liftIO $ print "handling changes"
  handleChanges $ detectFSChanges oldRemote newRemote newLocal


runTree   :: (Monad m, Functor m) => ((l,SubPath) -> m b) -> FSTree l -> m (FSTree b)
runTree f = runBottomUp f . fmap dropRootName . labelWithSubPaths
    where
      dropRootName (l,(_:sp)) = (l,sp)
      -- Drop the name of the root directory from our FSTree (since in the remoteTree
      -- this name of the user                          )


handleChanges         :: ( MonadResource m, Failure HttpException m
                         , MonadIO m, MonadBaseControl IO m) =>
                        FSChanges -> ActionT m ()
handleChanges changes = do
  runTree handleConflict . toHandleConflicts $ changes
  liftIO $ print "Deleting stuff"
  runTree deleteLocalFile' . toDeleteLocal    $ changes
  liftIO $ print "Downloading stuff:"
  liftIO $ print $ toDownload changes
  runTree downloadFile   . toDownload        $ changes
  liftIO $ print "patching stuff "
  runTree patchFile      . toPatchLocal      $ changes

  runTree deleteRemote   . toDeleteRemote    $ changes
  runTree uploadFile     . toUpload          $ changes
  runTree patchRemote    . toPatchRemote     $ changes
  return ()




downloadFile                 :: ( MonadResource m, Failure HttpException m
                                , MonadBaseControl IO m) =>
                               (FileIdent,SubPath) -> ActionT m ()
downloadFile (fi,sp)
             | isFile fi = toRemotePath' sp >>= getFile
             | otherwise = return ()






-- downloadFile :: ( MonadResource m, Failure HttpException m
--                 , MonadBaseControl IO m) =>
--                (Change FileIdent, SubPath) -> ActionT m ()
patchFile _ = return ()

patchRemote _ = return ()


uploadFile :: ( MonadResource m, Failure HttpException m
              , MonadIO m, MonadBaseControl IO m) =>
             (FSChange, SubPath) -> ActionT m ()
uploadFile (c,sp)
    | FI.isDirectory . newFileIdent $ c = toRemotePath' sp >>= createRemoteDirectory
    | otherwise                         = do
                                            rp <- toRemotePath' sp
                                            lp <- toLocalPath'  sp
                                            putFile' lp fi rp
        where
          fi = oldFileIdent c
          -- TODO
          createRemoteDirectory rp = return ()



--------------------------------------------------------------------------------

-- handleConflict                         :: Monad m =>
--                                           (Conflict FileIdent,SubPath) -> ActionT m ()
-- handleConflict ((Conflict loc rem),sp) = return () -- TODO

handleConflict _ = return ()

--------------------------------------------------------------------------------


deleteLocalFile      :: (Functor m, MonadIO m) => FileIdent -> Path -> ActionT m ()
deleteLocalFile fi p = atomicallyWrite p $
                       \fp -> protectedByFI fi fp "deleteLocalFile" (removeFile fp)

deleteLocalFile'         :: (Functor m, MonadIO m) => (FileIdent, SubPath) -> ActionT m ()
deleteLocalFile' (fi,sp) = toRemotePath' sp >>= deleteLocalFile fi


createLocalDirectory   :: (Functor m, MonadIO m) => Path -> ActionT m ()
createLocalDirectory p = toLocalPath p >>= liftIO . createDirectory





deleteRemote _ = return ()


atomicallyWrite    :: (Functor m, MonadIO m) =>
                           Path -> (FilePath -> IO (Either ErrorDescription a))
                                -> ActionT m ()
atomicallyWrite p h = do
                        fp <- toLocalPath p
                        mn <- liftIO $ atomicallyWriteIO fp (h fp)
                        case mn of
                          Left err -> liftIO $ print err
                          Right _  -> return ()


-- redownloadFile      :: ( MonadResource m, Failure HttpException m
--                        , MonadBaseControl IO m) FileIdent -> Path -> ActionT m ()
-- redownloadFile fi p = atomicallyWrite p $ do
--                         \fp -> protectedByFI fi fp "redownloadFile" redownload'
--     where
--       redownload' = return ()
