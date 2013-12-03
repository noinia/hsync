{-# Language  FlexibleContexts #-}
module HSync.Client.RemoteEvents where

import Control.Failure

import HSync.Client.Import


import HSync.Client.ActionT
import HSync.Client.Actions

-- import HSync.Common.Types
import HSync.Common.DateTime(DateTime)
-- import HSync.Common.FileIdent
import HSync.Common.Notification

import Network.HTTP.Conduit( HttpException(..) )









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



noConflict e t = return True --TODO


conflictsLocal         :: Monad m =>
                          Path -> Maybe FileIdent -> DateTime -> ActionT m Bool
conflictsLocal p mfi t = return False -- TODO!!







handleIncomingConflict _ = return ()
