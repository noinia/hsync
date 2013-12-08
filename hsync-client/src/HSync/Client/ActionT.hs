module HSync.Client.ActionT where

import Data.Conduit(ResourceT)
import Data.Default


import HSync.Common.DateTime(DateTime)
import HSync.Common.FSTree(FSTree)

import HSync.Client.Import
import HSync.Client.Sync(Sync)

import Yesod.Client




import qualified HSync.Client.Sync as S

--------------------------------------------------------------------------------
-- | A sync is YesodClient

type ActionT = YesodClientMonadT Sync


type Action = ActionT (ResourceT IO)


getSync :: Monad m => ActionT m Sync
getSync = clientInstance


runActionT          :: Functor m => ActionT m a -> Sync -> m a
runActionT act sync = evalYesodClientT act sync def


instance IsYesodClient Sync where
    type YesodServer Sync = HSyncServer
    serverAppRoot = S.serverAddress
    server   _    = def
    manager       = S.httpManager



remoteTree :: (Functor m, Monad m) => ActionT m (FSTree DateTime)
remoteTree = S.remoteTree <$> getSync

toLocalPath   :: (Functor m, Monad m) => Path -> ActionT m FilePath
toLocalPath p = flip S.toLocalPath p <$> getSync

toLocalPath'    :: (Functor m, Monad m) => SubPath -> ActionT m FilePath
toLocalPath' sp = flip S.toLocalPath' sp <$> getSync


toRemotePath   :: (Functor m, Monad m) => FilePath -> ActionT m Path
toRemotePath fp = flip S.toRemotePath fp <$> getSync

toRemotePath'    :: (Functor m, Monad m) => SubPath -> ActionT m Path
toRemotePath' sp = flip S.toRemotePath' sp <$> getSync
