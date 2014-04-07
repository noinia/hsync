{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
module HSync.Server.AcidState where

import Prelude
import Control.Monad.IO.Class(MonadIO(..))

import Data.Acid(AcidState, Update, Query,
                 EventState, EventResult,
                 QueryEvent, UpdateEvent)
import Data.Acid.Advanced(query', update')

--------------------------------------------------------------------------------
--- Using acid state from an arbitrary monad


class HasAcidState m st where
  getAcidState :: m (AcidState st)

queryAcid      :: forall event m.
                  ( Functor m
                  , MonadIO m
                  , QueryEvent event
                  , HasAcidState m (EventState event)
                  ) =>
                  event
                  -> m (EventResult event)
queryAcid event = do as <- getAcidState
                     query' (as :: AcidState (EventState event)) event


updateAcid      :: forall event m.
                   ( Functor m
                   , MonadIO m
                   , UpdateEvent event
                   , HasAcidState m (EventState event)
                   ) =>
                   event
                   -> m (EventResult event)
updateAcid event = do as <- getAcidState
                      update' (as :: AcidState (EventState event)) event
