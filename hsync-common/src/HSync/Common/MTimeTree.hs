{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.MTimeTree( MTimeTree
                             , readMTimeTree
                             , fileIdentOf
                             , updateFileIdent
                             , module HSync.Common.TimedFSTree
                             ) where


import Control.Monad.IO.Class(MonadIO(..))

import HSync.Common.DateTime(DateTime, modificationTime)
import HSync.Common.FileIdent(HasFileIdent(..))
import HSync.Common.FSTree.Zipper( fsTreeZipperAt , tree )
import HSync.Common.TimedFSTree
import HSync.Common.Types(SubPath)

import qualified HSync.Common.FileIdent as FI

--------------------------------------------------------------------------------

-- | A FSTree with modification times
type MTimeTree = TimedFSTree DateTime


-- | Read a MTreeFSTree from disk
readMTimeTree         :: (MonadIO m, Functor m) => FilePath -> m (Maybe MTimeTree)
readMTimeTree baseDir = readTimedFSTree baseDir modificationTime

-- | get the fileIdent of a certain file in the tree
fileIdentOf      :: SubPath -> Maybe MTimeTree -> FI.FileIdent
fileIdentOf p mt = toFileIdent . fmap tree $
                     mt >>= \t -> fsTreeZipperAt t () p

-- | Set a new fileIdent for the file indicated by Supath. If the new file ident is
-- nonExistent, i.e. we delete the file. the first parameter is the time at which
-- we do this delete. If this is not a delete, the first parameter is not inspected.
-- If the new file/directory does not exist yet in the tree, it is created.
updateFileIdent                   :: DateTime
                                  -> SubPath -> FI.FileIdent
                                  -> MTimeTree -> Maybe MTimeTree
updateFileIdent d p FI.NonExistent   = delete p d
updateFileIdent _ p (FI.File d)      = Just
                                     . insertOrAdjustLabel p (Left undefined) (const d)
updateFileIdent _ p (FI.Directory d) = Just
                                     . insertOrAdjustLabel p (Right undefined) (const d)


instance HasFileIdent (File DateTime) where
  toFileIdent = FI.File . fileLabel

instance HasFileIdent (Directory fl (DirectoryLabel DateTime)) where
  toFileIdent = FI.Directory . directoryLabel . dirLabel

instance HasFileIdent (FSTree DateTime (DirectoryLabel DateTime)) where
  toFileIdent (F t) = toFileIdent t
  toFileIdent (D t) = toFileIdent t
