{-# LANGUAGE LambdaCase #-}
module HSync.Server.Handler.ViewTree( getViewTreeR
                                    , getViewStateR
                                    ) where

import           Data.Maybe
import           Data.Either
import           HSync.Common.TimedFSTree
import           HSync.Common.FSTree
import           HSync.Server.Import hiding (fileName)

-- import           Text.Blaze(ToMarkup(..))

import           HSync.Server.FileSystemState
import           HSync.Server.AcidState
import           HSync.Server.User
import           HSync.Server.AcidSync(QueryFSState(..))

import           HSync.Server.Handler.FileActions(getTreeOf,getFileR)


import qualified Data.List as L
import qualified Data.Map as M

--------------------------------------------------------------------------------

-- | Get the MTimeTree directly from the files directory
getViewTreeR   :: Path -> Handler TypedContent
getViewTreeR p = getTreeOf p >>= \case
    Nothing -> getFileR p -- if p is not a dir, then it is either a file or nothing
                          -- Let the the getFileR handler distinguish between them.
    Just t  -> fmap toTypedContent . defaultLayout . displayDir ViewTreeR p . unTree $ t

-- | Get the MTimeTree by reading the FSState.
getViewStateR   :: Path -> Handler Html
getViewStateR p = queryDirectory p >>= \case
    Left err        -> defaultLayout $ [whamlet| err |]
    Right (Left f)  -> defaultLayout $ displayFile ViewStateR p f
    Right (Right d) -> defaultLayout $ displayDir  ViewStateR p d


--------------------------------------------------------------------------------

-- | Query Acid state for the MTimeTree corresponding to the input path
queryDirectory   :: Path -> Handler (Either Text (Either File' Directory'))
queryDirectory p = selectPath <$> queryAcid QueryFSState
  where
    selectPath = maybe (Left $ noUserMsg p) selectDir . M.lookup (owner p) . unM
    selectDir  = maybe (Left $ noDirMsg p)  Right . findAt (subPath p) . unTree

noDirMsg   :: Path -> Text
noDirMsg p = "No such file or directory: " <> showT (subPath p)

noUserMsg   :: Path -> Text
noUserMsg p = "No such user: " <> unUI (owner p)

--------------------------------------------------------------------------------
-- | Widgets

displayFile       :: Show m
                  => (Path -> Route HSyncServer) -- ^ Route constructor
                  -> Path
                  -> File m (FileData FileLabel) -> Widget
displayFile r p f = $(widgetFile "viewFile")


displayDir         :: (Show m, Show a)
                   => (Path -> Route HSyncServer) -- ^ Route constructor
                   -> Path  -- ^ Path to the directory we are displaying
                   -> Directory m a
                   -> Widget
displayDir r p dir = $(widgetFile "viewDirectory")
  where
    route   :: IsFileOrDirectory t => t x y -> Route HSyncServer
    route x = r (p { subPath = subPath p ++ [name x]} )
              -- append the name to the path

displayPathWith         :: (Path -> Route HSyncServer) -> Path -> Widget
displayPathWith route p = $(widgetFile "path")
  where
    ancestors  = zip ancestors' texts
    ancestors' = map (\sp -> p { subPath = sp}) . L.inits . subPath $ p
    texts      :: [Text]
    texts      = "/" : subPath p
