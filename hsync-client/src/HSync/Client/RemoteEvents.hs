module HSync.Client.RemoteEvents where

import HSync.Client.ActionT

import HSync.Client.Actions


handleNotification                  :: ( MonadResource m, Failure HttpException m
                                       , MonadBaseControl IO m) =>
                    Notification -> ActionT m ()
handleNotification (Notification e)






handleEvent (FileAdded p)           = getFile p
handleEvent (FileRemoved p fi)      = deleteFile (fi,subPath p)
handleEvent (FileUpdated p fi)      =
handleEvent (FileMoved f fi t)      = error "handleEvent: not implemented yet"

handleEvent (DirectoryAdded p)      = createLocalDirectory p

handleEvent (DirectoryRemoved p fi) =
handleEvent (DirectoryUpdated p)    = error "handleEvent: not implemented yet"
handleEvent (DirectoryMoved f t)    = error "handleEvent: not implemented yet"



createLocalDirectory p = return ()
