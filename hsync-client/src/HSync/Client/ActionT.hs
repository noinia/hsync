{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module HSync.Client.ActionT where

import Control.Applicative

import Control.Monad.Reader.Class
import Control.Monad.Reader


import Control.Monad.State.Class(get)

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Failure

import Data.Conduit(ResourceT)

import Data.Data(Data,Typeable)
import Data.Default
import Data.Text.Encoding(encodeUtf8)


import HSync.Common.DateTime(DateTime)
import HSync.Common.MTimeTree
import HSync.Common.Header(HClientId(..), asHeader)

import HSync.Client.Import

import HSync.Client.AcidSync
import HSync.Client.Sync(Sync)

import Network.HTTP.Conduit(HttpException)

import Yesod.Client


import qualified HSync.Client.Sync as S

--------------------------------------------------------------------------------
-- | A sync is a yesod client

instance IsYesodClient Sync where
    type YesodServer Sync = HSyncServer
    serverAppRoot = S.serverAddress
    server   _    = def
    manager       = S.httpManager

    defaultRequestModifier sync = let ci = S.clientIdent sync in
                                  addRequestHeader $ asHeader HClientId ci

--------------------------------------------------------------------------------
-- | A monad for running Sync Actions


-- | The ActionT transformer adds the AcidState to the yesodClientMonad
-- transformer
newtype ActionT acid m a = ActionT { unActionT ::
                                        ReaderT acid (YesodClientT Sync m) a
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


instance ( MonadResource m
         , MonadBaseControl IO m
         , Failure HttpException m
         ) =>
         MonadYesodClient Sync (ActionT acid) m where
  runGetRoute r     = liftYT $ runGetRoute r
  runPostRoute r s  = liftYT $ runPostRoute r s
  runDeleteRoute r  = liftYT $ runDeleteRoute r


-- | Lift a YesodClientT m action into a ActionT m action
liftYT      :: YesodClientT Sync m a -> ActionT acid m a
liftYT yAct = ActionT . ReaderT $ (\_ -> yAct)

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

-- | Our base monad for our computations
type SyncBaseMonad = ResourceT IO

-- | Shortcut for the Action
type Action = ActionT AcidSync SyncBaseMonad

------------------------------

-- | Get the sync for this action
getSync :: Action Sync
getSync = liftYT clientInstance

-- | Get the client state
getYesodClientState :: Action YesodClientState
getYesodClientState = liftYT get

-- | Get the AcidSync itself
getAcidSync :: Action AcidSync
getAcidSync = ActionT ask

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
