module HSync.Client.LocalEvents where

import HSync.Client.Import

import HSync.Client.ActionT
import HSync.Client.Actions
import HSync.Client.Sync(Sync)

import System.FSNotify

import qualified System.FSNotify as FSN

-- | should we use the time?
handleEvent                 :: FSN.Event -> Action ()
handleEvent (Added fp _)    = putFileOrDir fp
handleEvent (Modified fp _) = return ()  -- TODO: We need the OLD fileident here
handleEvent (Removed fp _)  = return ()  --       same here


-- syncUpstream :: ( MonadResource m, Failure HttpException m
--                       , MonadBaseControl IO m) =>
--                       DateTime -> Path -> ActionT m ()
