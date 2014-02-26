module HSync.Client.TemporaryIgnored where

import Control.Applicative
import Control.Monad.IO.Class(liftIO)

import Data.Monoid

import Control.Concurrent.STM(atomically)
import Control.Concurrent.STM.TVar

import Data.Set(Set)

import HSync.Client.Sync(TemporaryIgnoreFiles)
import HSync.Client.ActionT(Action, getActionState)


import qualified Data.Set  as S


initializeTemporaryIgnored :: IO (TVar TemporaryIgnoreFiles)
initializeTemporaryIgnored = newTVarIO mempty

isTemporarilyIgnored    :: FilePath -> Action Bool
isTemporarilyIgnored fp = getActionState >>= liftIO . isIgnored
  where
    isIgnored v = S.member fp <$> readTVarIO v

temporarilyIgnore    :: FilePath -> Action ()
temporarilyIgnore fp = getActionState >>= liftIO . atomically . add
  where
    add = flip modifyTVar (S.insert fp)

unIgnore    :: FilePath -> Action ()
unIgnore fp = getActionState >>= liftIO . atomically . delete
  where
    delete = flip modifyTVar (S.delete fp)
