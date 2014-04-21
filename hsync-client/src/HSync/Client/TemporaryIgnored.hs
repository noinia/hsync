module HSync.Client.TemporaryIgnored( initializeTemporaryIgnored
                                    , isTemporarilyIgnored
                                    , temporarilyIgnore
                                    , unIgnoreIn
                                    , unIgnore
                                    ) where


import Prelude hiding (FilePath)

import Control.Applicative
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.STM(atomically)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

import Data.Monoid
import Data.Set(Set)


import Filesystem.Path.CurrentOS

import HSync.Client.ActionT(Action, getActionState)
import HSync.Client.Logger
import HSync.Client.Sync(TemporaryIgnoreFiles)


import qualified Data.Set  as S


initializeTemporaryIgnored :: IO (TVar TemporaryIgnoreFiles)
initializeTemporaryIgnored = newTVarIO mempty

isTemporarilyIgnored    :: FilePath -> Action Bool
isTemporarilyIgnored fp = getActionState >>= liftIO . isIgnored
  where
    -- isIgnored v = S.member fp <$> readTVarIO v
    isIgnored v = do s <- readTVarIO v
                     print s
                     return $ S.member fp s

temporarilyIgnore    :: FilePath -> Action ()
temporarilyIgnore fp = getActionState >>= liftIO . atomically . add
  where
    add = flip modifyTVar (S.insert fp)


-- | Unignore the file after waiting `delay` microseconds
--  This function starts a new thread to wait, so it immediately
-- returns.
unIgnoreIn          :: Int -> FilePath -> Action ()
unIgnoreIn delay fp = getActionState >>= \v -> liftIO . const (return ()) . forkIO $
                           threadDelay delay
                        >> unIgnore' fp v

unIgnore    :: FilePath -> Action ()
unIgnore fp = getActionState >>= unIgnore' fp



unIgnore'    :: MonadIO m => FilePath -> TVar TemporaryIgnoreFiles -> m ()
unIgnore' fp = liftIO . atomically . delete
  where
    delete = flip modifyTVar (S.delete fp)
