module HSync.Client.TreeActions where





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
  runTree deleteFile     . toDeleteLocal     $ changes
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
                               FileIdent -> Path -> ActionT m ()
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


deleteFile :: MonadIO m => Path -> ActionT m ()
deleteFile _ = return () -- TODO, fix this


deleteRemote _ = return ()
