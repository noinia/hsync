{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.TimedFSTree( TimedFSTree
                               , readTimedFSTree
                               , update
                               , delete
                               , replaceSubTree
                               , adjustLabel

                               , addDir
                               , addFile

                               , DirectoryLabel(..)
                               , updateDirLabel
                               , fromFileLabel

                                 -- Reexports:
                               , FSTree(..), File(..), Directory(..)
                               , emptyDirectory , isDir , isFile
                               , label
                               ) where

import Control.Applicative((<$>))
import Control.Monad.IO.Class(MonadIO(..))

import Data.Aeson.TH
import Data.Data(Data, Typeable)

import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.DateTime(DateTime, AsDateTime(..))
import HSync.Common.FSTree( FSTree(..), File(..), Directory(..)
                          , update, adjust, replace
                          , readFSTree, labelBottomUp, updateLabel
                          , addFileAt, addDirAt
                          , emptyDirectory , isDir , isFile , label
                          )

import HSync.Common.Types(FileName, SubPath)

import qualified HSync.Common.FSTree    as FT

--------------------------------------------------------------------------------

-- | A directory label that stores the same info as a file label, together with
-- the last time this subtree was modified.
data DirectoryLabel fl = DirectoryLabel { directoryLabel :: fl
                                        , modTime        :: DateTime -- The last time this
                                                                     -- subtree was
                                                                    -- updated.
                                        }
                       deriving  (Show,Eq,Ord,Data,Typeable)

$(deriveJSON defaultOptions ''DirectoryLabel)
$(deriveSafeCopy 0 'base ''DirectoryLabel)

-- | Given either a file label or a directory label Compute an updated
-- directory label.
updateDirLabel :: AsDateTime fl =>
                  Either fl (DirectoryLabel fl)
               -> DirectoryLabel fl -> DirectoryLabel fl
updateDirLabel (Left x)   = updateDirLabel' (toDateTime x)
updateDirLabel (Right dl) = updateDirLabel' (modTime dl)

updateDirLabel'                         :: DateTime
                                        -> DirectoryLabel fl -> DirectoryLabel fl
updateDirLabel' t' (DirectoryLabel l t) = DirectoryLabel l $ maximum [t,t']


fromFileLabel   :: AsDateTime fl => fl -> DirectoryLabel fl
fromFileLabel l = DirectoryLabel l $ toDateTime l

--------------------------------------------------------------------------------

-- | A FSTree where directories store information about the last time they are
-- modified.
type TimedFSTree fl = FSTree fl (DirectoryLabel fl)


-- | Given a basedir and a file labelling function. Compute a FSTree. The
-- directory labels are computed using the same file labelling function.
readTimedFSTree             :: (Functor m, MonadIO m, AsDateTime fl) =>
                               FilePath -- ^ base dir
                            -> (FilePath -> m fl) -- ^ how to ocmpute a file label
                            -> m (Maybe (TimedFSTree fl))
readTimedFSTree base fileLF = fmap (labelBottomUp f) <$> readFSTree base fileLF dirLF
  where
    dirLF fp                       = fromFileLabel <$> fileLF fp
    f (DirectoryLabel l t) dls fls = let fts = map toDateTime fls
                                         dts = map modTime dls in
                                     DirectoryLabel l $ maximum (t : fts ++ dts)

------------------------------

-- | Given a path and a file label, update the node at that path with that time.
-- this also updates all mtimes on the path to that node.
adjustLabel      :: AsDateTime fl => SubPath -> fl -> TimedFSTree fl -> TimedFSTree fl
adjustLabel p fl = adjust updateDirLabel p treeF
  where
    dirLF oldDl = updateDirLabel' (modTime oldDl) (fromFileLabel fl)
    treeF       = updateLabel (const fl) dirLF
                  -- if we have a file, the label is just the file label
                  -- itself. If we have a directory, we construct a directory
                  -- label from fl. The (new) modification time is either the
                  -- old modification time, or we can derive it form the new
                  -- label.

-- | Replace a subtree and propagate the new labels upwards.
replaceSubTree :: AsDateTime fl => SubPath
               -> TimedFSTree fl -- ^ subtree
               -> TimedFSTree fl -> TimedFSTree fl
replaceSubTree = replace updateDirLabel

-- | Delete a subtree
delete      :: AsDateTime fl => SubPath
            -> fl -- ^ Time at which we delete the file/dir
            -> TimedFSTree fl -> Maybe (TimedFSTree fl)
delete p fl = FT.delete updateDirLabel (Left fl) p

-- | Add the directory at the specified path
addDir :: AsDateTime fl => SubPath
       -> Directory fl (DirectoryLabel fl)
       -> TimedFSTree fl
       -> TimedFSTree fl
addDir = addDirAt updateDirLabel

-- | Add a file at the specified path
addFile :: AsDateTime fl => SubPath
        -> File fl
        -> TimedFSTree fl
        -> TimedFSTree fl
addFile = addFileAt updateDirLabel
