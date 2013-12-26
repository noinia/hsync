{-# LANGUAGE FlexibleInstances #-}
{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.MTimeTree where

import Control.Applicative((<$>))
import Control.Monad.State.Class(modify, get)

import Data.Aeson.TH
import Data.Data(Data, Typeable)

import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.DateTime(DateTime, modificationTime)
import HSync.Common.FSTree
import HSync.Common.FSTreeZipper
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
              deriving  (Show,Eq,Ord,Data,Typeable)

$(deriveJSON defaultOptions ''DirMTime)
$(deriveSafeCopy 0 'base ''DirMTime)


-- | Compute a new DirMTime from
updateDT                  :: DateTime -> DirMTime -> DirMTime
updateDT t (DirMTime l s) = DirMTime l $ maximum [s, Just t]


updateDT' (DirMTime _ s') (DirMTime l s) = DirMTime l $ maximum [s, s']


-- | Read a dir modificationtime. We do not read the recursive times yet.

readDirMTime fp = flip DirMTime Nothing <$> modificationTime fp



-- | Read a MTreeFSTree from disk
readMTimeTree baseDir = fmap (labelBottomUp (\(DirMTime l re) dls fls -> DirMTime l
                          (maximum $ re :  map Just fls ++ map subtreeMTime dls)
                        )) <$> readFSTree baseDir modificationTime readDirMTime



-- | Given a path and a datetime, update the node at that path with that time.
-- this also updates all mtimes on the path to that node.
updateMTime         :: SubPath -> DateTime -> Maybe MTimeFSTree -> Maybe MTimeFSTree
updateMTime p dt mt = mt >>= flip fsTreeZipper ()
                          >>= goTo p
                          >>= return . tree . goToRoot .
                              updateAndPropagate updateDT updateDT' f
  where
    f = updateLabel (const dt) (const $ DirMTime dt (Just dt))


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
