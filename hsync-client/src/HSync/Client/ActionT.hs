{-# LANGUAGE DeriveDataTypeable #-}
module HSync.Client.ActionT where

import Control.Applicative

import Control.Monad.Reader.Class
import Control.Monad.Reader


import Control.Monad.State.Class(get)

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Acid(AcidState)
import Data.Acid.Advanced(query',update')
import Data.Acid.Memory.Pure(Event, QueryEvent, EventResult, EventState)

import Data.Conduit(ResourceT)

import Data.Data(Data,Typeable)
import Data.Default

import HSync.Common.DateTime(DateTime)
import HSync.Common.MTimeTree


import HSync.Client.Import

import HSync.Client.AcidSync
import HSync.Client.Sync(Sync)

import Yesod.Client


import qualified HSync.Client.Sync as S

--------------------------------------------------------------------------------
-- | A sync is a yesod client

instance IsYesodClient Sync where
    type YesodServer Sync = HSyncServer
    serverAppRoot = S.serverAddress
    server   _    = def
    manager       = S.httpManager


--------------------------------------------------------------------------------
-- | A monad for running Sync Actions


-- | The ActionT transformer adds the AcidState to the yesodClientMonad
-- transformer
newtype ActionT acid m a = ActionT { unActionT ::
                                        ReaderT acid (YesodClientMonadT Sync m) a
                                   }

instance Monad m => Monad (ActionT acid m) where
  return = ActionT . return
  (ActionT m) >>= f = let f' = unActionT . f in
                      ActionT $ m >>= f'

instance MonadTrans (ActionT acid) where
  lift = ActionT . lift . lift

instance MonadIO m => MonadIO (ActionT acid m) where
  liftIO = ActionT . liftIO

instance Functor m => Functor (ActionT acid m) where
  fmap f = ActionT . fmap f . unActionT

instance (Functor m, Monad m) => Applicative (ActionT acid m) where
  pure = ActionT . pure
  (ActionT f) <*> (ActionT m) = ActionT $ f <*> m

------------------------------

-- | Given an action, a sync and an AcidState, run the action
runActionT               :: Functor m => ActionT acid m a -> Sync -> acid -> m a
runActionT act sync acid = runActionTWithClientState def sync acid act

-- | Given yesod client state, sync, AcidState and an action. Run the action
runActionTWithClientState                         :: Functor m =>
                                                     YesodClientState ->
                                                     Sync ->
                                                     acid ->
                                                     ActionT acid m a ->
                                                     m a
runActionTWithClientState st sync acid (ActionT a) = evalYesodClientT
                                                     (runReaderT a acid)
                                                     sync st

--------------------------------------------------------------------------------
-- | The instantiated monad we will use to run our actions

-- | A single type that collects everything that we acidize
data AcidSync = AcidSync { remoteTreeAcid :: AcidState MTimeTreeState
                         }

-- | Our base monad for our computations
type SyncBaseMonad = ResourceT IO

-- | Shortcut for the Action
type Action = ActionT AcidSync SyncBaseMonad

------------------------------

-- | Get the sync for this action
getSync :: Action Sync
getSync = ActionT . ReaderT $ (\_ -> clientInstance)

-- | Get the client state
getYesodClientState :: Action YesodClientState
getYesodClientState = ActionT . ReaderT $ (\_ -> get)

-- | Get the AcidSync itself
acidSync :: Action AcidSync
acidSync = ActionT ask

------------------------------

queryAcid                   :: QueryEvent ev =>
                               (AcidSync -> AcidState (EventState ev)) ->
                               ev ->
                               Action (EventResult ev)
queryAcid field queryEvent = do
                               acidState <- field <$> acidSync
                               query' acidState queryEvent

-- | Get the remote tree
remoteTree :: Action (Maybe MTimeFSTree)
remoteTree = queryAcid remoteTreeAcid PeekMTimeTree



------------------------------
-- | Something on paths

toLocalPath   :: Path -> Action FilePath
toLocalPath p = flip S.toLocalPath p <$> getSync

toLocalPath'    :: SubPath -> Action FilePath
toLocalPath' sp = flip S.toLocalPath' sp <$> getSync

toRemotePath   :: FilePath -> Action Path
toRemotePath fp = flip S.toRemotePath fp <$> getSync

toRemotePath'    :: SubPath -> Action Path
toRemotePath' sp = flip S.toRemotePath' sp <$> getSync
