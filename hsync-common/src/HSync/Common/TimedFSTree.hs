{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.TimedFSTree( FileData(..)

                               , TimedFSTree(..)

                               , MTimeTree
                               , fileIdentOf
                               , readMTimeData
                               , readMTimeTree


                               , addByFileIdent
                               , deleteByFileIdent
                               , updateByFileIdent

                               , Max(..)
                               ) where

import Control.Applicative((<$>))
import Control.Monad.IO.Class(MonadIO(..))

import Data.Aeson.TH
import Data.Data(Data, Typeable)

import Data.Semigroup

import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.DateTime(DateTime, AsDateTime(..), modificationTime)
import HSync.Common.FSTree

import HSync.Common.FileIdent(FileIdent, HasFileIdent(..))

import HSync.Common.Notification(Notification)

import HSync.Common.Types(FileName, SubPath)

import qualified HSync.Common.FileIdent as FI


--------------------------------------------------------------------------------

-- | The second arugment is the modification time
data FileData a = FileData { extractData          :: a
                           , modificationTimeData :: DateTime
                           }
                       deriving  (Show,Eq,Ord,Data,Typeable)

$(deriveJSON defaultOptions ''FileData)
$(deriveSafeCopy 0 'base ''FileData)

$(deriveJSON defaultOptions ''Max)
$(deriveSafeCopy 0 'base ''Max)

instance Measured (Max DateTime) (FileData a) where
  measure (FileData _ m) = Max m

instance HasFileIdent (File m (FileData a)) where
  toFileIdent = FI.File . modificationTimeData . dataValue

instance HasFileIdent (Directory m (FileData a)) where
  toFileIdent = FI.Directory . modificationTimeData . dataValue


--------------------------------------------------------------------------------
-- | A tree with just the modification times

newtype TimedFSTree a = FSTree { unTree :: Directory (Max DateTime) (FileData a) }
                        deriving (Show,Eq,Ord,Data,Typeable,HasFileIdent)

$(deriveJSON defaultOptions ''TimedFSTree)
$(deriveSafeCopy 0 'base ''TimedFSTree)


withDir              :: ( Directory (Max DateTime) (FileData t) ->
                          Directory (Max DateTime) (FileData a)
                        )
                     -> TimedFSTree t -> TimedFSTree a
withDir f (FSTree d) = FSTree $ f d

--------------------------------------------------------------------------------

type MTimeTree = TimedFSTree ()

-- | Read an MTimeTree from disk. Returns Nothing if the path does not point to
-- a directory on the filesystem.
readMTimeTree :: (Functor m, MonadIO m) => FilePath -> m (Maybe MTimeTree)
readMTimeTree = fmap (fmap FSTree) . flip readDirectory readMTimeData

-- | Read the Modification time data for a single file
readMTimeData    :: (Functor m, MonadIO m) => FilePath -> m (FileData ())
readMTimeData fp = (FileData ()) <$> modificationTime fp


-- | get the fileIdent of a certain file in the tree
fileIdentOf   :: SubPath -> MTimeTree -> FileIdent
fileIdentOf p = toFileIdent . findAt p . unTree

-- | Given a subpath and a fileIdent. Add a new item (i.e. file or directory)
-- to the MTimeTree. This operation assumes that all parents of the new item
-- already exist in the tree.
addByFileIdent                     :: SubPath -> FileIdent -> MTimeTree -> MTimeTree
addByFileIdent _ FI.NonExistent    = id
addByFileIdent p (FI.File dt)      = let (sp,n) = andLast p in withDir $
                                     addFileAt      sp (file n $ FileData () dt)
addByFileIdent p (FI.Directory dt) = let (sp,n) = andLast p in withDir $
                                     addDirectoryAt sp (emptyDirectory n $ FileData () dt)

-- | Delete the item, i.e. file or directory at the indicated sub path. If the
-- item is a directory, this removes that entire subtree. The given dateTime is used
-- to (recursive) update the Modification times.
deleteByFileIdent :: SubPath -> DateTime -> FileIdent -> MTimeTree -> MTimeTree
deleteByFileIdent _ _  FI.NonExistent   = id
deleteByFileIdent p dt (FI.File _)      = let (sp,n) = andLast p in withDir $
                                          deleteFileAt      sp n (Max dt)
deleteByFileIdent p dt (FI.Directory _) = let (sp,n) = andLast p in withDir $
                                          deleteDirectoryAt sp n (Max dt)

updateByFileIdent                :: SubPath -> FileIdent -> MTimeTree -> MTimeTree
updateByFileIdent p (FI.File dt) = let g f    = f { fileData = FileData () dt }
                                   in withDir $ updateFileAt p g
updateByFileIdent _ _            = error "updateByFileIdent: Only files are supported."

--------------------------------------------------------------------------------
