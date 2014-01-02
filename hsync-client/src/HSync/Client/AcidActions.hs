module HSync.Client.AcidActions where

import Control.Applicative

import Data.Acid(AcidState)
import Data.Acid.Advanced(query',update')
import Data.Acid.Memory.Pure( Event, QueryEvent,UpdateEvent
                            , EventResult, EventState)

import HSync.Common.MTimeTree
import HSync.Common.Types
import HSync.Common.DateTime(DateTime)


import HSync.Client.ActionT
import HSync.Client.AcidSync

import qualified HSync.Common.FileIdent as FI

--------------------------------------------------------------------------------

-- | Run a acid Qury in the Action monad
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


-- | Update the tree state
updateTreeState   :: (MTimeFSTree -> Maybe MTimeFSTree) -> Action ()
updateTreeState f = updateTreeState' (>>= f)


updateTreeState'   :: (Maybe MTimeFSTree -> Maybe MTimeFSTree) -> Action ()
updateTreeState' f = do
                      mt <- serverTreeState
                      let mt' = f mt
                      updateAcid remoteTreeAcid (UpdateReplaceFull mt')


-- | Get what we think is the FileIdent for the file indicated by path.
expectedFileIdent   :: Path -> Action FI.FileIdent
expectedFileIdent p = fileIdentOf (subPath p) <$> serverTreeState


--------------------------------------------------------------------------------

-- | Given a path, the old file ident and the new file ident. Update this file
-- ident in the remote-tree state that we maintain.
updateFileIdent         :: Path -> FI.FileIdent -> FI.FileIdent -> Action ()
updateFileIdent p oldFi = updateFileIdent' p undefined oldFi

-- | Given a path, a potential removal time, the old fi and the new fi, update the
--  file ident that we store in the remote-tree state.
updateFileIdent'                 :: Path -> DateTime
                                 -> FI.FileIdent -> FI.FileIdent -> Action ()
updateFileIdent' p d oldFi newFi = updateTreeState $ updateFI (subPath p) d oldFi newFi

-- | updateFI determines which function we should run to update the remote tree state.
updateFI                                   :: SubPath -> DateTime ->
                                              FI.FileIdent -> FI.FileIdent ->
                                              (MTimeFSTree -> Maybe MTimeFSTree)
updateFI _ _ FI.NonExistent FI.NonExistent = error "updateFI: NonExistent."
updateFI p _ FI.NonExistent newFi          = let n  = last p
                                                 dt = FI.getDateTime newFi
                                                 f  = File n dt
                                                 d  = emptyDirectory n $ dirMTime dt
                                             in Just . if FI.isFile newFi
                                                       then addFile p f else addDir p d
updateFI p d _              FI.NonExistent = delete p d
updateFI p _ _              newFi          = let dt = FI.getDateTime newFi in
                                             Just . updateMTime p dt
