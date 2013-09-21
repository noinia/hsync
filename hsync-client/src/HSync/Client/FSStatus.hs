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
                           , unchanged    :: FSTree l
                           }
                  deriving (Show,Eq)

instance Functor FSStatus where
    fmap f = tmap (fmap f) (fmap (fmap f))


-- | Get the files with their new labels that have been updated
updated' :: FSStatus l -> FSTree l
updated' = newFromChange . updated

newFromChange :: FSTree (Change l) -> FSTree l
newFromChange = fmap (fromJust . new)



tmap                          :: (FSTree a -> FSTree b) ->
                                 (FSTree (Change a) -> FSTree (Change b)) ->
                                 FSStatus a -> FSStatus b
tmap f g (FSStatus a d up un) = FSStatus (f a) (f d) (g up) (f un)


instance Default (FSStatus l) where
    def = FSStatus NoFiles NoFiles NoFiles NoFiles


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
fsStatus oldTree newTree = FSStatus nt dt upt unt
    where
      mt  = mergeTree newTree oldTree
      nt  = leftTree  . newInLeft                 $ mt
      dt  = rightTree . newInRight                $ mt
      upt = leftTree  . withChanges . newerInLeft $ mt
      unt = leftTree  . intersection              $ mt

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

-- | Given the oldRemote tree, the new remote tree, and the newLocal tree,
-- determine what we have to do to synchronize everything.
--
-- FIXME:  The toDeletes are not enitrely ok yet. Fix that
detectChanges                              :: Ord l =>
                                              FSTree l -> FSTree l -> FSTree l ->
                                              Changes (Change l) l
detectChanges oldRemote newRemote newLocal = Changes
    { toDownload        = leftTree     $ added remoteStatus
                                         `notIn`
                                         newLocal
                          -- we download everything that is newly added in the remote
                          -- tree
    , toDeleteLocal     = leftTree     $ deleted remoteStatus
                                         `identicalIn`
                                         unchanged localStatus
                          -- We can only safely delete files that we have not touched
                          -- ourselves
    , toPatchLocal      = leftTree'    $ updated remoteStatus
                                         `inTree`
                                         unchanged localStatus
                          -- Everything that has changed remotely, and we have not touched
                          -- locally
    , toUpload          = leftChanges  $ added localStatus
                                         `notIn`
                                         newRemote
                          -- We upload everything that is new in the local tree
                          -- and does not exist in the remote tree.
    , toDeleteRemote    = rightChanges $ unchanged remoteStatus
                                         `identicalIn`
                                         deleted localStatus
                          -- We remotely delete stuff that we have deleted. More precicely
                          -- we only delete it, if the file that we deleted is identical
                          -- to the one still in the remote tree.

                          -- TODO: Fix the issue with directories that did not match
                          -- themselves, but some child matched.
    , toPatchRemote     = rightChanges $ updated' localStatus
                                         `inTree`
                                         unchanged remoteStatus
                          -- We remotely patch stuff if we changed it locally, and nobody
                          -- else changed the file in the meantime
    , toHandleConflicts = undefined
    }
    where
      remoteStatus = fsStatus oldRemote newRemote
      localStatus  = fsStatus oldRemote newLocal

      leftChanges  = leftTree . withChanges
      rightChanges = rightTree . withChanges



      -- leftChanges' = leftTree . map fromMerge id
      leftTree'    = newFromChange . leftTree


-- | Get a mergetree representing the items that are identical in both trees
identicalIn     :: Eq l => FSTree l -> FSTree l -> MergeTree l l
identicalIn l r = identical $  mergeTree l r


-- | Get a mergetree representing the items that simply *occur* in both subtrees
-- (but may differ)
inTree     :: FSTree l -> FSTree r -> MergeTree l r
inTree l r = intersection $ mergeTree l r


-- | Get a mergetree representing everything that *does* occur in the left subtree
-- but does *not* occur in the right subtree.
notIn      :: FSTree l -> FSTree r -> MergeTree l r
notIn l r = difference $ mergeTree l r




-- | this should be identical to uncahnged (and thus we should be able to remove it)
notInS                               :: FSTree a -> FSStatus l -> FSTree a
notInS l (FSStatus add del chs same) = leftTree $ l'' `notIn` chs
    where
      l'   = leftTree $ l  `notIn` add
      l''  = leftTree $ l' `notIn` del


--------------------------------------------------------------------------------
-- | FSChanges

type FSChanges = Changes FSChange FileIdent

data FSChange = FSChange { oldFileIdent :: FileIdent
                         , newFileIdent :: FileIdent
                         }

-- | convert a ChangeFileIdent to a FSChange
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
fromMerge                             :: Merge l l -> Merge (Change l) (Change l)
fromMerge (Merge n (Just l) (Just r)) = Merge n (Just l') (Just r')
    where
      l' = mkChange l (label' r)
      r' = mkChange r (label' l)
fromMerge (Merge n _ _)               = Merge n Nothing Nothing


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
                           , File "deletedInRemote" 1
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
                            -- , File "deletedInRemote" 1
                            ]

test = detectChanges oldT remoteT localT
