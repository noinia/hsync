{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.ViewTree where

import Data.Maybe
import Data.Either
import HSync.Common.TimedFSTree
import HSync.Server.Import

import HSync.Server.FileSystemState
import HSync.Server.AcidState
import HSync.Server.AcidSync(QueryFSState(..))

import HSync.Server.Handler.FileActions(getTreeOf)
import HSync.Server.Handler.Auth(protectRead, protectWrite)

import qualified Data.Map as M

--------------------------------------------------------------------------------

-- | Get the MTimeTree directly from the files directory
getViewTreeR   :: Path -> Handler Html
getViewTreeR p = protectRead p "vierTree" $ getTreeOf p >>= \case
    Nothing -> notFound
    Just t  -> defaultLayout $ [whamlet| #{show t} |]

getViewStateR   :: Path -> Handler Html
getViewStateR p = protectRead p "viewState" $ queryDirectory p >>= \case
    Left err -> defaultLayout $ [whamlet| err |]
    Right t  -> defaultLayout $ [whamlet| #{show t} |]

-- | Query Acid state for the MTimeTree corresponding to the input path
queryDirectory   :: Path -> Handler (Either Text (TimedFSTree FileLabel))
queryDirectory p = selectPath <$> queryAcid QueryFSState
  where
    msg        = "no such user: " <> unUI (owner p)
    selectPath = maybe (Left msg) selectDir . M.lookup (owner p) . unM
    selectDir  = Right -- TODO: Select the acutal subdirectory corresp to p
