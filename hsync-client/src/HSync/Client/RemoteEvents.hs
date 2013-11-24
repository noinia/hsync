module HSync.Client.RemoteEvents where

import HSync.Client.ActionT

import HSync.Client.Actions












handleNotification                       :: ( MonadResource m, Failure HttpException m
                                            , MonadBaseControl IO m) =>
                                            Notification -> ActionT m ()
handleNotification (Notification e ci t) = protect (noConflict e t)
                                                   (handleEvent e)
                                                   (handleIncomingConflict n)



noConflict e t =


conflictsLocal         :: Path -> Maybe FileIdent -> DateTime -> ActionT m Bool
conflictsLocal p mfi t = return False -- TODO!!


handleEvent (FileAdded p)           = getFile p
handleEvent (FileRemoved p fi)      = deleteLocalFile fi p
handleEvent (FileUpdated p fi)      = return ()
handleEvent (FileMoved f fi t)      = error "handleEvent: not implemented yet"

handleEvent (DirectoryAdded p)      = createLocalDirectory p

handleEvent (DirectoryRemoved p fi) = return ()
handleEvent (DirectoryUpdated p)    = error "handleEvent: not implemented yet"
handleEvent (DirectoryMoved f t)    = error "handleEvent: not implemented yet"




handleIncomingConflict _ = return ()



-- deleteLocalFile fi fp = protect
