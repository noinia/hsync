{-# LANGUAGE FlexibleInstances #-}
module HSync.Common.MTimeTree( MTimeTree
                             , readMTimeTree
                             , fileIdentOf
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

instance HasFileIdent (File DateTime) where
  toFileIdent = FI.File . fileLabel

instance HasFileIdent (Directory fl (DirectoryLabel DateTime)) where
  toFileIdent = FI.Directory . directoryLabel . dirLabel

instance HasFileIdent (FSTree DateTime (DirectoryLabel DateTime)) where
  toFileIdent (F t) = toFileIdent t
  toFileIdent (D t) = toFileIdent t
