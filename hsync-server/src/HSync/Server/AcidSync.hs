{-# Language TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module HSync.Server.AcidSync where

import Prelude

import Control.Exception.Base(bracket)
import Control.Monad.Reader.Class
import Control.Monad.State.Class(modify)
import Control.Monad.IO.Class(MonadIO(..))

import Data.Acid(AcidState, Update, Query,
                 makeAcidic, openLocalState)
import Data.Acid.Advanced(update')
import Data.Acid.Local(createCheckpointAndClose)

import HSync.Common.Notification(Notification)
import HSync.Common.TimedFSTree


import HSync.Server.FileSystemState
import HSync.Server.Settings(Extra(..))

--------------------------------------------------------------------------------

-- | A single type that collects everything that we acidize
data AcidSync = AcidSync { fsState :: AcidState FSState
                         }

--------------------------------------------------------------------------------
-- | The acidic operations

-- | Get the thing we are actually storing
queryFSState :: Query FSState FSState
queryFSState = ask


updateReplaceFull   :: FSState -> Update FSState ()
updateReplaceFull t = modify (const t)

updateNewNotification   :: Notification -> Update FSState ()
updateNewNotification n = modify (updateNotification n)

$(makeAcidic ''FSState [ 'queryFSState
                       , 'updateReplaceFull
                       , 'updateNewNotification
                       ])

--------------------------------------------------------------------------------

withAcidSync        :: Extra
                    -> (AcidSync -> IO a)
                    -> IO a
withAcidSync conf f = do
                        let baseDir = extraFilesDir conf
                        -- Get a blank copy of the of the FSTree. This is only used if
                        -- there is no acidious state yet.
                        blankFSS <- newFSState' baseDir
                        -- Start/open Acid state
                        bracket (openLocalState blankFSS)
                                (createCheckpointAndClose)
                                (f . AcidSync)

newFSState' b = print "woei" >> newFSState b

--------------------------------------------------------------------------------

notificationUpdate            :: MonadIO m => AcidSync -> Notification -> m ()
notificationUpdate acidSync n = let fsStateAcid = fsState acidSync in
                                update' fsStateAcid (UpdateNewNotification n)
