module HSync.Client.FSStatus( FSStatus(..)
                            , fsStatus
                            ) where


import Data.Monoid
import Data.Maybe

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
                           , updated      :: FSTree (Change l)
                           }
                  deriving (Show,Eq)

instance Functor FSStatus where
    fmap f = tmap (fmap f) (fmap (fmap f))


tmap                      :: (FSTree a -> FSTree b) ->
                             (FSTree (Change a) -> FSTree (Change b)) ->
                             FSStatus a -> FSStatus b
tmap f g (FSStatus a d u) = FSStatus (f a) (f d) (g u)


instance Default (FSStatus l) where
    def = FSStatus NoFiles NoFiles NoFiles


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
      nt = leftTree  . newInLeft                 $ mt
      dt = rightTree . newInRight                $ mt
      ut = leftTree  . withChanges . newerInLeft $ mt

--------------------------------------------------------------------------------



data Change l = Change { old :: Maybe l
                       , new :: Maybe l
                       }
                deriving (Show,Read,Eq)

instance Functor Change where
    fmap f (Change o n) = Change (fmap f o) (fmap f n)


change o n = Change (Just o) (Just n)


data Changes c l = Changes { toDownload        :: FSTree l
                           , toDeleteLocal     :: FSTree l
                           , toPatchLocal      :: FSTree l

                           , toUpload          :: FSTree c
                           , toDeleteRemote    :: FSTree c
                           , toPatchRemote     :: FSTree c

                           , toHandleConflicts :: FSTree c
                           }
             deriving (Show,Eq)

-- | fmap like but with functions on FSTrees instead of the items themselves
cmap                                 :: (FSTree a -> FSTree b) ->
                                        (FSTree c -> FSTree d) ->
                                        Changes c a -> Changes d b
cmap f g (Changes d dl pl u dr pr c) = Changes (f d) (f dl) (f pl)
                                               (g u) (g dr) (g pr)
                                               (g c)

--------------------------------------------------------------------------------

detectChanges                              :: Ord l =>
                                              FSTree l -> FSTree l -> FSTree l ->
                                              Changes (Change l) l
detectChanges oldRemote newRemote newLocal = Changes
    { toDownload        = leftTree  $ added   remoteStatus `notIn`  newLocal
    , toDeleteLocal     = leftTree  $ deleted remoteStatus `inTree` newLocal
    , toPatchLocal      = leftTree' $ updated remoteStatus `notInS` localStatus

    , toUpload          = leftChanges $ added   localStatus  `notIn`  newRemote
    , toPatchRemote     = leftTree    $ updated localStatus  `notInS` remoteStatus
    , toDeleteRemote    = leftChanges $ deleted localStatus  `notIn`  newRemote

    , toHandleConflicts = undefined
    }
    where
      remoteStatus = fsStatus oldRemote newRemote
      localStatus  = fsStatus oldRemote newLocal

      leftChanges  = leftTree . withChanges
      leftTree'    = fromChange' . leftTree

      fromChange' :: FSTree (Change l) -> FSTree l
      fromChange' = fmap (fromJust . new)


inTree     :: FSTree l -> FSTree r -> MergeTree l r
inTree l r = intersection $ mergeTree l r

notIn      :: FSTree l -> FSTree r -> MergeTree l r
notIn l r = difference $ mergeTree l r


notInS      :: FSTree a -> FSStatus l -> MergeTree a (Change l)
notInS l (FSStatus add del chs) = l'' `notIn` chs
    where
      l'   = leftTree $ l  `notIn` add
      l''  = leftTree $ l' `notIn` del




--------------------------------------------------------------------------------
-- | FSChanges

type FSChanges = Changes FSChange FileIdent

data FSChange = FSChange { oldFileIdent :: FileIdent
                         , newFileIdent :: FileIdent
                         }


fromChange              :: Change FileIdent -> FSChange
fromChange (Change o n) = FSChange (f o) (f n)
    where
      f = fromMaybe FI.NonExistent


-- | convert a FSTree of DateTimes to a FSTRee of FileIdents
withFileIdents :: FSTree DateTime -> FSTree FileIdent
withFileIdents = gmap FI.Directory FI.File


withFileIdents' :: FSTree (Change DateTime) -> FSTree (Change FileIdent)
withFileIdents' = gmap (fmap FI.Directory) (fmap FI.File)


fromChanges :: Changes (Change DateTime) DateTime -> FSChanges
fromChanges = cmap withFileIdents (fmap fromChange . withFileIdents')



detectFSChanges                              :: FSTree DateTime ->
                                                FSTree DateTime ->
                                                FSTree DateTime ->
                                                FSChanges
detectFSChanges oldRemote newRemote newLocal =
    fromChanges $ detectChanges oldRemote newRemote newLocal

--------------------------------------------------------------------------------
-- |


-- withFileIdents :: MergeTree DateTime DateTime -> MergeTree FileIdent FileIdent
-- withFileIdents = fmap (updateMerge f f)
--     where
--       f (Item dt t chs) = Item (toFi t dt) t (map withFileIdents' chs)

--       toFi   :: FSType -> DateTime -> FileIdent
--       toFi F = FI.File
--       toFi D = FI.Directory







-- | Convert the label type to a `(Change l)`. The items in the right tree have
-- *their* label set as the new label!. So which one is true the right one
-- should be chosen by selecting either the left or the right tree.
withChanges :: MergeTree l l -> MergeTree (Change l) (Change l)
withChanges = fmap fromMerge

-- | Update a Merge l l to a Merge (Change l) (Change l). The items in the left
-- tree have *their* label set as the new label, the items in the right tree
-- have *their* label set as the new label!. So which one is true the right one
-- should be chosen by selecting either the left or the right tree.
fromMerge               :: Merge l l -> Merge (Change l) (Change l)
fromMerge (Merge n l r) = Merge n l' r'
    where
      l' = mkChange l (label' r)
      r' = mkChange r (label' l)


-- | create a Change object from a FSItem and an oldLabel. In particular,
-- the new label in `mkChange item oldLabel` is the label in item, the
-- old label is the one specified.
mkChange                          :: FSItemData l ->
                                     l -> FSItemData (Change l)
mkChange (Item new' t chs) old' = Item (change old' new') t chs'
    where
      chs' = map (fmap (Change Nothing . Just)) chs




--------------------------------------------------------------------------------
-- | Testing stuff

oldT = FSTree $
        Directory "root" 0 [ File "onlyOld"    1
                           , File "both"        2
                           , Directory "subdir" 3 [ File "foo" 4
                                                  ]
                           , Directory "baz" 100 []
                           ]

remoteT = FSTree $
          Directory "root" 0 [ File "onlyNewRemote"   10
                             , File "both"        2
                             , Directory "subdir" 3 [ File "foo" 5
                                                    , File "bar" 6
                                                    ]
                             , File "baz" 101
                             ]

localT = FSTree $
         Directory "root" 0 [ File "newLocal"    50
                            , File "both"        2
                            , Directory "subdir" 3 [ File "foo" 4
                                                   , File "localNewSub" 6
                                                   ]
                            , Directory "baz" 100 [ ]
                            , Directory "localbaz" 200 [ File "X" 201
                                                       ]
                            ]

test = detectChanges oldT remoteT localT
