{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.ViewTree where

import Data.Maybe
import Data.Either
import HSync.Common.TimedFSTree
import HSync.Common.FSTree
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
    Left err        -> defaultLayout $ [whamlet| err |]
    Right (Left f)  -> defaultLayout $ [whamlet| #{show f} |]
    Right (Right d) -> defaultLayout $ [whamlet| #{show d} |]

-- type Directory' = Directory (Max DateTime) FileLabel

-- | Query Acid state for the MTimeTree corresponding to the input path
queryDirectory   :: Path -> Handler (Either Text (Either File' Directory'))
queryDirectory p = selectPath <$> queryAcid QueryFSState
  where
    selectPath = maybe (Left noUserMsg) selectDir . M.lookup (owner p) . unM
    selectDir  = maybe (Left noDirMsg)  Right . findAt (subPath p) . unTree
    noUserMsg  = "No such user: " <> unUI (owner p)
    noDirMsg   = "No such file or directory: " <> showT (subPath p)
