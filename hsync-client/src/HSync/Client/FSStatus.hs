module HSync.Client.FSStatus( FSStatus(..)
                            , fsStatus
                            , fsStatus'
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


data FSStatus l = FSStatus { added        :: Maybe (FSTree l)
                           , deleted      :: Maybe (FSTree l)
                           , updated      :: Maybe (MergeTree l l)
                           }
                  deriving (Show,Read,Eq)


instance Default (FSStatus l) where
    def = FSStatus Nothing Nothing Nothing


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
fsStatus oldTree newTree = FSStatus nt dt ut
    where
      mt = mergeTree newTree oldTree
      nt = fmap leftTree  . newInLeft  $ mt
      dt = fmap rightTree . newInRight $ mt
      ut = newerInLeft mt

-- | In case there is no old tree everyting is added
fsStatus'          :: Ord l => Maybe (FSTree l) -> FSTree l -> FSStatus l
fsStatus' mOld new = maybe (def { added = Just new }) (flip fsStatus new) mOld


--------------------------------------------------------------------------------

data Change = Change { oldFileIdent :: FileIdent
                     , newFileIdent :: FileIdent
                     }
            deriving (Show,Read,Eq)



type LocalTree = FSTree DateTime

type RemoteTree = FSTree Change


data Changes = Changes { toDownload        :: Maybe LocalTree
                       , toDeleteLocal     :: Maybe LocalTree
                       , toPatchLocal      :: Maybe LocalTree

                       , toUpload          :: Maybe RemoteTree
                       , toDeleteRemote    :: Maybe RemoteTree
                       , toPatchRemote     :: Maybe RemoteTree

                       , toHandleConflicts :: Maybe RemoteTree
                       }
             deriving (Show,Read,Eq)




detectChanges oldRemote newRemote newLocal = Changes
    { toDownload        = undefined
    , toDeleteLocal     = undefined --deleted remoteStatus               `notIn` newLocal
    , toPatchLocal      = undefined --- (rightTree $ updated remoteStatus) `notInS` localStatus


    , toUpload          = added localStatus    `notInR` newRemote

    , toPatchRemote     = undefined -- TODO: we need the remote FI here
--                                  updated localStatus  `notInS` remoteStatus
    , toDeleteRemote    = undefined -- deleted localStatus  `notIn` newRemote
    , toHandleConflicts = undefined
    }
    where
      remoteStatus = fsStatus' oldRemote newRemote
      localStatus  = fsStatus' oldRemote newLocal

      dl = mkLocal $ added remoteStatus `notIn` newLocal



notIn      :: Maybe (FSTree l) -> FSTree l -> Maybe (MergeTree l l)
notIn ml r = ml >>= difference . flip mergeTree r




mkLocal :: Maybe (MergeTree DateTime DateTime) -> Maybe LocalTree
mkLocal = fmap leftTree

mkRemote :: Maybe (MergeTree DateTime DateTime) -> Maybe RemoteTree
mkRemote = fmap (leftTree . withChanges)





withChanges :: MergeTree DateTime DateTime -> MergeTree Change Change
withChanges = withChanges' . withFileIdents


withFileIdents :: MergeTree DateTime DateTime -> MergeTree FileIdent FileIdent
withFileIdents = fmap (updateMerge f f)
    where
      f i@(Item dt t chs) = Item (toFI i) t (map withFileIdents' chs)

withFileIdents' :: FSTree DateTime -> FSTree FileIdent
withFileIdents' = gmap FI.Directory FI.File




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



--------------------------------------------------------------------------------




notInR :: Maybe (FSTree DateTime) -> FSTree DateTime -> Maybe RemoteTree
notInR = undefined -- notInWithF toChange



-- notInWithF        :: (Merge l l -> Merge l' r') ->
--                      Maybe (FSTree l) -> FSTree l -> Maybe (FSTree l')
-- notInWithF f ml r = ml >>= \l -> projectLeftWith (fmap (fmap f) . difference) l r



-- toChange               :: Merge DateTime DateTime -> Merge Change DateTime
-- toChange (Merge n l r) = Merge n l' r
--     where
--       l' = mkChange l r


-- -- | The Left tree is considered the NEW tree, the right tree the Old tree
-- mkChange :: FSItemData DateTime -> FSItemData DateTime -> FSItemData Change
-- mkChange l@(Item _ t chs) r = Item change t chs'
--     where
--       change = Change (toFI r) (toFI l)
--       chs'   = map f chs

--       f :: FSTree DateTime -> FSTree Change
--       f = fmap (Change FI.NonExistent) . gmap FI.Directory FI.File
--       -- First we set the labels to be FileIdents instead of just the
--       -- date time. Then, since these items occur only in this Item
--       -- we set the label to a Change value, where the old FI is set to
--       -- Nothing

--  -- = Item (toFi t l) t $ map (gmap FI.Directory FI.File) chs




toFI(Item l t _) = toFi' t l
    where
      toFi'   :: FSType -> DateTime -> FileIdent
      toFi' F = FI.File
      toFi' D = FI.Directory




-- withFileIdent                :: FSItemData DateTime -> FSItemData FileIdent
-- withFileIdent (Item l t chs) = Item (toFi t l) t $ map (gmap FI.Directory FI.File) chs



notins = undefined
