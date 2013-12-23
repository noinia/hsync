module HSync.Client.AcidActions where

import Control.Applicative

import Data.Acid(AcidState)
import Data.Acid.Advanced(query',update')
import Data.Acid.Memory.Pure( Event, QueryEvent,UpdateEvent
                            , EventResult, EventState)

import HSync.Common.FSTree
import HSync.Common.MTimeTree
import HSync.Common.Types


import HSync.Client.ActionT
import HSync.Client.AcidSync

--------------------------------------------------------------------------------

-- | Run a acid Query in the Action monad
queryAcid                   :: QueryEvent ev =>
                               (AcidSync -> AcidState (EventState ev)) ->
                               ev ->
                               Action (EventResult ev)
queryAcid field queryEvent = do
                               acidState <- field <$> getAcidSync
                               query' acidState queryEvent


-- | Run an acid update in the Action monad
updateAcid                   :: UpdateEvent ev =>
                                (AcidSync -> AcidState (EventState ev)) ->
                                ev ->
                                Action (EventResult ev)
updateAcid field updateEvent = do
                               acidState <- field <$> getAcidSync
                               update' acidState updateEvent



-- | returns a Maybe MTimeFSTree that the client *thinks* represents the state
-- of the filesystem on the server.
serverTreeState :: Action (Maybe MTimeFSTree)
serverTreeState = queryAcid remoteTreeAcid QueryMTimeTree


-- | Replaces the subtree at sp in the remoteTree with st
replaceMTimeTree       :: SubPath -> MTimeFSTree -> Action ()
replaceMTimeTree sp st = updateAcid remoteTreeAcid (UpdateReplaceMTimeTree sp st)
