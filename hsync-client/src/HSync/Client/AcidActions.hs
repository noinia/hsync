module HSync.Client.AcidActions where

import Control.Applicative

import Data.Acid(AcidState)
import Data.Acid.Advanced(query',update')
import Data.Acid.Memory.Pure( Event, QueryEvent,UpdateEvent
                            , EventResult, EventState)

import HSync.Common.FSTree
import HSync.Common.MTimeTree
import HSync.Common.Types
import HSync.Common.FileIdent


import HSync.Client.ActionT
import HSync.Client.AcidSync

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
serverFileState    :: SubPath -> Action FI.FileIdent
serverFileState sp = toFileIdent <$> serverTreeState


--------------------------------------------------------------------------------

updateFileIdent                     :: Path -> FI.FileIdent -> Action ()
updateFileIdent p FI.NonExistent    = return () -- TODO
updateFileIdent p (FI.File dt)      = updateFileIdent' (subPath p) dt
updateFileIdent p (FI.Directory dt) = updateFileIdent' (subPath p) dt
-- TODO, distinguish between adds and updates

updateFileIdent' p dt = updateTreeState (Just . updateMTime p dt)




expectedFileIdent   :: Path -> Action FI.FileIdent
expectedFileIdent p = serverTreeState >>= \mt -> case mt of
                        Nothing -> error "expectedFileIdent: no status tree"
                        Just t  -> return $ fileIdentOf (subPath p) t
