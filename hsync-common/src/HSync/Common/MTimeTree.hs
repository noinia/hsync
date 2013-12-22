{-# LANGUAGE FlexibleInstances #-}
{-# Language TemplateHaskell #-}
module HSync.Common.MTimeTree where

import Control.Applicative((<$>))

import Data.Aeson.TH

import HSync.Common.DateTime(DateTime, modificationTime)
import HSync.Common.FSTree
import HSync.Common.Types(FileName, SubPath)

import qualified HSync.Common.FileIdent as FI

--------------------------------------------------------------------------------

-- | A FSTree with modification times
type MTimeFSTree = FSTree DateTime DirMTime

-- | The directory labels consist of their local modification time, and
-- possibly (if known) the last modification time in its subtree.
data DirMTime = DirMTime { localMTime   :: DateTime  -- My own modification time
                         , subtreeMTime :: Maybe DateTime  -- The last modification time
                                                           -- in this subtree
                         }
              deriving  (Show,Eq,Ord)

$(deriveJSON defaultOptions ''DirMTime)


-- | Read a dir modificationtime. We do not read the recursive times yet.

readDirMTime fp = flip DirMTime Nothing <$> modificationTime fp






readMTimeTree baseDir = fmap (labelBottomUp (\(DirMTime l re) dls fls -> DirMTime l
                          (maximum $ re :  map Just fls ++ map subtreeMTime dls)
                        )) <$> readFSTree baseDir modificationTime readDirMTime


class HasFileIdent c where
  toFileIdent :: c -> FI.FileIdent

instance HasFileIdent (File DateTime) where
  toFileIdent = FI.File . fileLabel

instance HasFileIdent (Directory fl DirMTime) where
  toFileIdent = FI.Directory . localMTime . dirLabel

instance HasFileIdent (FSTree DateTime DirMTime) where
  toFileIdent (F t) = toFileIdent t
  toFileIdent (D t) = toFileIdent t

instance HasFileIdent a => HasFileIdent (Maybe a) where
  toFileIdent = maybe FI.NonExistent toFileIdent
