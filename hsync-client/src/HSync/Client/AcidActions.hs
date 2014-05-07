module HSync.Client.AcidActions where

import Control.Applicative

import Data.Acid(AcidState)
import Data.Acid.Advanced(query',update')
import Data.Acid.Memory.Pure( Event, QueryEvent,UpdateEvent
                            , EventResult, EventState)


import HSync.Common.Types
import HSync.Common.DateTime(DateTime)
import HSync.Common.FileIdent(FileIdent, isNonExistent)
import HSync.Common.TimedFSTree(MTimeTree, fileIdentOf)

import HSync.Client.Import(protect)
import HSync.Client.AcidSync
import HSync.Client.ActionT

import qualified HSync.Common.FileIdent as FI

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


--------------------------------------------------------------------------------

-- | Get what we think is the FileIdent for the file indicated by path.
expectedFileIdent   :: Path -> Action FI.FileIdent
expectedFileIdent p = (maybe FI.NonExistent (fileIdentOf (subPath p)))
                      <$> serverTreeState


-- | returns a Maybe MTimeTree that the client *thinks* represents the state
-- of the filesystem on the server.
serverTreeState :: Action (Maybe MTimeTree)
serverTreeState = queryAcid remoteTreeAcid QueryMTimeTree


replaceServerStateBy    :: Maybe MTimeTree -> Action ()
replaceServerStateBy mt = updateAcid remoteTreeAcid (ReplaceFull mt)



addByFileIdent       :: SubPath -> FileIdent -> Action ()
addByFileIdent sp fi = updateAcid remoteTreeAcid $ AddByFileIdent sp fi


deleteByFileIdent         :: Path -> DateTime -> FileIdent -> Action ()
deleteByFileIdent p dt fi = updateAcid remoteTreeAcid $ DeleteByFileIdent (subPath p) dt fi

updateByFileIdent       :: SubPath -> FileIdent -> Action ()
updateByFileIdent sp fi = updateAcid remoteTreeAcid $ UpdateByFileIdent sp fi


setByFileIdent      :: Path -> FileIdent -> Action ()
setByFileIdent p fi = protect (isNonExistent <$> expectedFileIdent p)
                              (addByFileIdent    (subPath p) fi)
                              (updateByFileIdent (subPath p) fi)
