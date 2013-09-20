module HSync.Client.FSStatus( FSStatus(..)
                            , fsStatus
                            ) where


import Data.Default

import HSync.Common.DateTime(DateTime)
import HSync.Common.FileIdent(FileIdent)
import HSync.Common.FSTree
import HSync.Client.MergeTree


import qualified HSync.Common.FileIdent as FI


--------------------------------------------------------------------------------
-- | A data type to represent the status of the file system. I..e which files
-- have been added, deleted, and which files have been updated.


data FSStatus l = FSStatus { added        :: FSTree l
                           , deleted      :: FSTree l
                           , updated      :: MergeTree l l
                           }
                  deriving (Show,Eq)


instance Default (FSStatus l) where
    def = FSStatus NoFiles NoFiles def


-- | fsStatus oldTree newTree computes the changes in the filesystem between
-- newTree and oldTree. It reports:

--  * which files have been added in the new tree (i.e. the ones that were not
--    there yet in oldTree),
--  * which files have been removed (i.e. the in oldTree
--    no longer in newTree), and
--  * the files that have been updated (i.e. the files that are in both trees,
--    but are newer in newTree than in oldTree).
--
--  Note: we assume that whenever files change, their labels change.
--  Note2: There is one type of change that we do not capture, namely when
--         the type of a file changes from a file to a directory or vice versa
--         without the label changing. However, by the previous note/assumption
--         such changes should not happen!
fsStatus                 :: Ord l => FSTree l -> FSTree l -> FSStatus l
fsStatus NoFiles newTree = def { added   = newTree }
fsStatus oldTree NoFiles = def { deleted = oldTree }
fsStatus oldTree newTree = FSStatus nt dt ut
    where
      mt = mergeTree newTree oldTree
      nt = leftTree  . newInLeft  $ mt
      dt = rightTree . newInRight $ mt
      ut = newerInLeft mt

--------------------------------------------------------------------------------

data Change = Change { oldFileIdent :: FileIdent
                     , newFileIdent :: FileIdent
                     }
            deriving (Show,Read,Eq)



type LocalTree = FSTree DateTime

type RemoteTree = FSTree Change


data Changes = Changes { toDownload        :: LocalTree
                       , toDeleteLocal     :: LocalTree
                       , toPatchLocal      :: LocalTree

                       , toUpload          :: RemoteTree
                       , toDeleteRemote    :: RemoteTree
                       , toPatchRemote     :: RemoteTree

                       , toHandleConflicts :: RemoteTree
                       }
             deriving (Show,Eq)




detectChanges oldRemote newRemote newLocal = Changes
    { toDownload        = leftTree $ added   remoteStatus `notIn`  newLocal
    , toDeleteLocal     = leftTree $ deleted remoteStatus `inTree` newLocal
    , toPatchLocal      = leftTree $ updated remoteStatus `notInS` localStatus


    , toUpload          = undefined --added localStatus    `notInR` newRemote

    , toPatchRemote     = undefined -- TODO: we need the remote FI here
--                                  updated localStatus  `notInS` remoteStatus
    , toDeleteRemote    = undefined -- deleted localStatus  `notIn` newRemote
    , toHandleConflicts = undefined
    }
    where
      remoteStatus = fsStatus oldRemote newRemote
      localStatus  = fsStatus oldRemote newLocal


inTree     :: FSTree l -> FSTree r -> MergeTree l r
inTree l r = intersection $ mergeTree l r

notIn      :: FSTree l -> FSTree l -> MergeTree l l
notIn l r = difference $ mergeTree l r

notInS = undefined



mkLocal :: MergeTree DateTime DateTime -> LocalTree
mkLocal = leftTree

mkRemote :: MergeTree DateTime DateTime -> RemoteTree
mkRemote = leftTree . withChanges





withChanges :: MergeTree DateTime DateTime -> MergeTree Change Change
withChanges = withChanges' . withFileIdents


withFileIdents :: MergeTree DateTime DateTime -> MergeTree FileIdent FileIdent
withFileIdents = fmap (updateMerge f f)
    where
      f i@(Item dt t chs) = Item (toFI i) t (map withFileIdents' chs)

withFileIdents' :: FSTree' DateTime -> FSTree' FileIdent
withFileIdents' = gmap' FI.Directory FI.File




withChanges'  :: MergeTree FileIdent FileIdent -> MergeTree Change Change
withChanges' = fmap fromMerge

fromMerge               :: Merge FileIdent FileIdent -> Merge Change Change
fromMerge (Merge n l r) = Merge n l' r'
    where
      l' = mkChange l (label' r)
      r' = mkChange r (label' l)


mkChange                          :: FSItemData FileIdent ->
                                     FileIdent -> FSItemData Change
mkChange (Item newFi t chs) oldFi = Item (Change oldFi newFi) t chs'
    where
      chs' = map (fmap (Change FI.NonExistent)) chs
