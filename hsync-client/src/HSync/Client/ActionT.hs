{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module HSync.Client.ActionT where

import Prelude hiding (FilePath)


import Control.Concurrent.STM.TVar(TVar)
import Control.Applicative

import Control.Monad.Reader.Class
import Control.Monad.Reader

import Control.Monad.State.Class
import Control.Monad.State

import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource(ResourceT, runResourceT)
import Control.Monad.IO.Class

import Data.Data(Data,Typeable)
import Data.Default
import Data.Text.Encoding(encodeUtf8)

import Filesystem.Path.CurrentOS

import HSync.Common.DateTime(DateTime)
import HSync.Common.MTimeTree
import HSync.Common.Header(HClientId(..), asHeader)

import HSync.Client.Import

import HSync.Client.AcidSync
import HSync.Client.Sync(Sync, TemporaryIgnoreFiles)

import Network.HTTP.Conduit(HttpException)

import Yesod.Client


import qualified HSync.Client.Sync as S

--------------------------------------------------------------------------------
-- | A sync is a yesod client

instance IsYesodClient Sync where
    type YesodServer Sync = HSyncServer
    serverAppRoot = S.serverAddress
    server   _    = undefined
    manager       = S.httpManager

    defaultRequestModifier sync = let ci = S.clientIdent sync in
                                  addRequestHeader $ asHeader HClientId ci




--------------------------------------------------------------------------------
-- | A monad for running Sync Actions


-- | The ActionT transformer adds a state transformer and a reader transformer
-- to the YesodClient monad transformer.
newtype ActionT s r m a = ActionT { unActionT ::
                                       StateT s (
                                         ReaderT r (
                                            YesodClientT Sync m
                                                   )
                                         ) a
                                  }
                          deriving (Functor,Applicative,Monad,MonadIO)

instance MonadTrans (ActionT s r) where
  lift = ActionT . lift . lift . lift


instance ( MonadResource m
         , MonadBaseControl IO m
         ) =>
         MonadYesodClient Sync (ActionT s r) m where
  runGetRoute r     = liftYT $ runGetRoute r
  runPostRoute r s  = liftYT $ runPostRoute r s
  runDeleteRoute r  = liftYT $ runDeleteRoute r


-- -- | Lift a YesodClientT m action into a ActionT m action
liftYT :: (Monad m, Functor m) => YesodClientT Sync m a -> ActionT s r m a
liftYT = ActionT . lift . lift
-- liftYT yAct = ActionT . StateT $ (\s -> ReaderT $ (\_ -> (\x -> (x,s)) <$> yAct))

----------------------------------------


-- | Run an action and explicitly supply the YesodClient state.
runActionTWithYST                       :: (Functor m, Monad m)
                                        => YesodClientState -- ^ yesod state
                                        -> Sync
                                        -> s    -- ^ state
                                        -> r    -- ^ reader
                                        -> ActionT s r m a
                                        -> m a
runActionTWithYST yst sync s r (ActionT a) = let rm = evalStateT a s
                                                 ym = runReaderT rm r
                                             in evalYesodClientT ym sync yst


-- | Run an action
runActionT              :: (Functor m, Monad m)
                        => ActionT s r m a -> Sync -> s -> r -> m a
runActionT act sync s r = runActionTWithYST def sync s r act


-- | ``Clone'' the current Action so we can run the given action in a new base action.

cloneInIO     :: MonadBaseControl IO m
              => ActionT ActionState ActionReader (ResourceT m) a
              -> Action (m a)
cloneInIO act = do
                  sync   <- getSync
                  yState <- getYesodClientState
                  s      <- getActionState
                  r      <- getActionReader
                  let ioA = runResourceT $
                              runActionTWithYST yState sync s r act
                  return ioA

--------------------------------------------------------------------------------
-- | The instantiated monad we will use to run our actions

-- | The mutable state in our actions
type ActionState  = TVar TemporaryIgnoreFiles

-- | The immutable state in our actions
type ActionReader = AcidSync

-- | Our base monad for our computations
type SyncBaseMonad = ResourceT IO

-- | Shortcut for the Action
type Action = ActionT ActionState ActionReader SyncBaseMonad

------------------------------

-- | Get the sync for this action
getSync :: Action Sync
getSync = liftYT clientInstance

-- | Get the client state
getYesodClientState :: Action YesodClientState
getYesodClientState = liftYT get

-- | Get the AcidSync itself
getAcidSync :: Action AcidSync
getAcidSync = getActionReader

getActionState :: Action ActionState
getActionState = ActionT get

getActionReader :: Action ActionReader
getActionReader = ActionT ask


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
