{-# Language TypeFamilies #-}
module HSync.Server.Handler.Acid where

import Data.Acid(AcidState)
import Data.Acid.Advanced(query',update')
import Data.Acid.Memory.Pure( Event, QueryEvent,UpdateEvent
                            , EventResult, EventState)

import HSync.Server.AcidSync
import HSync.Server.FileSystemState(FSState)
import HSync.Server.Import

-- --------------------------------------------------------------------------------


-- -- | Run a acid Query in the Handler monad
-- queryAcid                   :: QueryEvent ev =>
--                                (AcidSync -> AcidState (EventState ev)) ->
--                                ev ->
--                                Handler (EventResult ev)
-- queryAcid field queryEvent = do
--                                acidState <- field <$> getAcidSync
--                                query' acidState queryEvent

-- -- | Run an acid update in the Handler monad
-- updateAcid                   :: UpdateEvent ev =>
--                                 (AcidSync -> AcidState (EventState ev)) ->
--                                 ev ->
--                                 Handler (EventResult ev)
-- updateAcid field updateEvent = do
--                                acidState <- field <$> getAcidSync
--                                update' acidState updateEvent

-- --------------------------------------------------------------------------------


-- getFSState :: Handler FSState
-- getFSState = queryAcid fsState (QueryFSState)


-- updateFSState   :: (FSState -> FSState) -> Handler ()
-- updateFSState f = getFSState >>= \t ->
--                     updateAcid fsState (ReplaceFull $ f t)


-- --updateNotification :: Notification ->
