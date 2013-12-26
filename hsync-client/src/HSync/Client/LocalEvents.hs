module HSync.Client.LocalEvents where

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.AcidActions
import HSync.Client.Sync(Sync)

import HSync.Common.Types

import System.FSNotify

import qualified System.FSNotify as FSN

-- | should we use the time?
handleEvent                 :: FSN.Event -> Action ()
handleEvent (Added fp _)    = putFileOrDir fp
handleEvent (Modified fp _) = do
                                p  <- toRemotePath fp
                                fi <- serverFileState $ subPath p
                                putUpdate fp fi p
handleEvent (Removed fp _)  = do
                                p  <- toRemotePath fp
                                fi <- serverFileState $ subPath p
                                deleteRemote fi p


-- syncUpstream :: ( MonadResource m, Failure HttpException m
--                       , MonadBaseControl IO m) =>
--                       DateTime -> Path -> ActionT m ()
