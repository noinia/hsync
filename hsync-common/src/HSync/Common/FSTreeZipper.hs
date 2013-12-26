{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.FSTreeZipper( FSTreeZipper
                                , fsTreeZipper
                                , fsTreeZipper'

                                , tree
                                , crumbs
                                , zipperState

                                , FSCrumb(..)
                                , Crumb(..)
                                , path

                                , readFSTreeZipper

                                , goUp
                                , goToRoot

                                , goToFile
                                , goToDir
                                , goToDirOrFile
                                , goTo
                                , goToNextFile

                                , update
                                , updateAndPropagate
                                , replace
                                , delete
                                ) where


import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO, MonadIO)

import Data.Aeson.TH
import Data.Data(Data, Typeable)

import Data.List(break)
import Data.Maybe(catMaybes, fromJust)
import Data.Text(Text)

import HSync.Common.FSTree
import HSync.Common.Types(FileName, SubPath)


import System.FilePath (takeFileName, dropTrailingPathSeparator, (</>))

import Data.SafeCopy(base, deriveSafeCopy)

import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | FSTree zipper

-- | A FSTree zipper with state. A FSTreeZipper fl dl zs represents a selected
-- FSTree fl dl, together with state zs.
type FSTreeZipper fl dl zs = (FSTree fl dl, [FSCrumb fl dl], zs)


fsTreeZipper   :: FSTree fl dl -> zs -> Maybe (FSTreeZipper fl dl zs)
fsTreeZipper t = Just . fsTreeZipper' t

fsTreeZipper'      :: FSTree fl dl -> zs -> FSTreeZipper fl dl zs
fsTreeZipper' t zs = (t,[],zs)


tree         :: FSTreeZipper fl dl zs -> FSTree fl dl
tree (t,_,_) = t

crumbs          :: FSTreeZipper fl dl zs -> [FSCrumb fl dl]
crumbs (_,bs,_) = bs

zipperState          :: FSTreeZipper fl dl zs -> zs
zipperState (_,_,zs) = zs


-- | The bread crumbs in our zipper, i.e. all information that we need at a
-- particular place in the tree. If the currently selected tree t is a
-- directory the context of t is represented by a DCrumb. If t is a file, it is
-- represented by a FCrumb.
--
-- The actual data is stored in the Crumb data type (see below).
data FSCrumb fl dl = DCrumb (Crumb dl (File fl)         (Directory fl dl))
                   | FCrumb (Crumb dl (Directory fl dl) (File fl))
                   deriving (Show,Read, Eq)


-- | A `Crumb dl unCh ch' represents the data in a crumb:
--
-- dl : the directory label of the directory containing the currently selected tree
-- unCh: The type of the unchanged items in the context. I.e. if the currently selected
--        tree is a file, then the subdirectories in the content are unchanged.
-- ch: The type of the changed items in the context. I.e. if the currently
--     selected tree t is a file, then the list of files of its parent (i.e. the
--     context) is split into two lists by: all files (ch's) left of t, and all files (ch's)
--     right of t.
data Crumb dl unCh ch = Crumb { dirName'    :: FileName
                              , dirLabel'   :: dl
                              , unchanged   :: [unCh]
                              , lefts       :: [ch]
                              , rights      :: [ch]
                              }
                      deriving (Show,Read,Eq)

$(deriveJSON defaultOptions ''FSCrumb)
$(deriveJSON defaultOptions ''Crumb)

------------------------------

-- | Given a FSCrumb, get the filename corresponding to this crumb.
name'            :: FSCrumb fl dl -> FileName
name' (DCrumb c) = dirName' c
name' (FCrumb c) = dirName' c

-- | Given a FSCrumb, get the label corresponding to this crumb.
label'            :: FSCrumb fl dl -> dl
label' (DCrumb c) = dirLabel' c
label' (FCrumb c) = dirLabel' c


-- | convert a crumb to a directory
toDir                      :: Crumb dl unCh ch ->
                              [Directory fl dl] -> [File fl] -> Directory fl dl
toDir (Crumb n dl _ _ _) = Directory n dl


-- | Get the path from the root to the currently selected tree item.
path          :: FSTreeZipper fl dl zs -> SubPath
path (t,bs,_) = let dir = map name' $ bs in
                reverse $ rootFileName t : dir

--------------------------------------------------------------------------------
-- | Reading a FSTreeZipper from Disk


-- | Read a FSTreeZipper from disk.
readFSTreeZipper              :: (Functor m, MonadIO m) =>
                                 FilePath ->
                                 (FilePath -> m fl) -> -- ^ how to ocmpute a file label
                                 (FilePath -> m dl) -> -- ^ how to compute a dir label
                                 m (Maybe (FSTreeZipper fl dl FilePath))
readFSTreeZipper baseDir fL dL = fmap (\t -> (t,[],baseDir)) <$> readFSTree baseDir fL dL


--------------------------------------------------------------------------------
-- | Moving in the zipper

goUp                         :: FSTreeZipper fl dl zs -> Maybe (FSTreeZipper fl dl zs)
goUp (F f, (FCrumb c):bs,zs) = let ds = unchanged c
                                   fs = lefts c ++ [f] ++ rights c in
                               Just (D $ toDir c ds fs, bs, zs)
goUp (D d, (DCrumb c):bs,zs) = let fs = unchanged c
                                   ds = lefts c ++ [d] ++ rights c in
                               Just (D $ toDir c ds fs, bs, zs)
goUp (_,[],zs)               = Nothing
goUp _                       = error "goUp: inconsistent zipper"


goToRoot            :: FSTreeZipper fl dl zs -> FSTreeZipper fl dl zs
goToRoot z@(_,[],_) = z
goToRoot z          = goToRoot . fromJust $ goUp z


-- | Given a filename, go to that *FILE* of the currently selected
-- subtree. Note that this operation fails (returns Nothing), if the filename
-- points to a directory.
goToFile                                          :: FileName ->
                                                     FSTreeZipper fl dl zs ->
                                                     Maybe (FSTreeZipper fl dl zs)
goToFile fn (D (Directory n dl ds fs), bs, zs) = case break (`hasFileName` fn) fs of
  (_,   [])    -> Nothing
  (lfs, f:rfs) -> Just (F f, FCrumb (Crumb n dl ds lfs rfs):bs,zs)
goToFile _ _                                   = Nothing -- if we are already at a file
                                                         -- we cannot go to a child

-- | Given a directoryname, go to that *DIRECTORY* of the currently selected
-- subtree. Note that this operation fails (returns Nothing), if the directory
-- name points to a file.
goToDir                                       :: FileName ->
                                                 FSTreeZipper fl dl zs ->
                                                 Maybe (FSTreeZipper fl dl zs)
goToDir dn (D (Directory n dl ds fs), bs, zs) = case break (`hasDirName` dn) ds of
  (_,   [])    -> Nothing
  (lds, d:rds) -> Just (D d, DCrumb (Crumb n dl fs lds rds):bs,zs)
goToDir _ _                                   = Nothing -- if we are already at a file
                                                         -- we cannot go to a child


-- | Give na name, of either a file or a directory. Go to that file/directory.
goToDirOrFile     :: FileName -> FSTreeZipper fl dl zs -> Maybe (FSTreeZipper fl dl zs)
goToDirOrFile n z = let dz = goToDir n z in case dz of
                      Just _  -> dz
                      Nothing -> goToFile n z


-- | Given a subpath, traverse the zipper along this path.
goTo     :: SubPath -> FSTreeZipper fs dl zs -> Maybe (FSTreeZipper fs dl zs)
goTo p z = foldl (\z' n -> z' >>= goToDirOrFile n) (return z) p



-- | Go to the next file in the file system. If we are currently on a
-- *directory* this operation fails.
goToNextFile                         :: FSTreeZipper fl dl zs ->
                                        Maybe (FSTreeZipper fl dl zs)
goToNextFile (F f, (FCrumb c):bs,zs) = case rights c of
  []      -> Nothing
  (f':rs) -> let c' = c { lefts  = lefts c ++ [f]
                        , rights = rs } in
             Just (F f', FCrumb c':bs,zs)
goToNextFile (F _, [], _)            = Nothing
goToNextFile _                       = Nothing

--------------------------------------------------------------------------------
-- | Modifying the zipper

-- | apply the given function on the currently selected subtree.
update               :: (FSTree fl dl -> FSTree fl dl) ->
                        FSTreeZipper fl dl zs -> FSTreeZipper fl dl zs
update f (t,bs,zs)
  | isDir t == isDir t' = (t',bs,zs) -- the FSTree is of the same type as before
  | otherwise           = case (t',bs) of -- FSTree changed of type
       (F _, (DCrumb (Crumb n l fs dl dr)):bs') ->
           -- Apparently the tree was a directory, which we have now deleted and replaced
           -- by a file. So we recombine the list of directories. And, assume that the new
           -- file is the first in the list of files.
           (t',FCrumb (Crumb n l (dl ++ dr) [] fs):bs',zs)
       (D _, (FCrumb (Crumb n l ds fl fr)):bs') ->
           -- Symmetric to the above
           (t',DCrumb (Crumb n l (fl ++ fr) [] ds):bs',zs)
  where
    t'       = f t

-- | updateAndPropagate updateLF updateDL f z updates the tree currently selected
--   using function f. Furthermore, it propagates the update of this tree
--   in the labels of the ancestors of this node using the functions updateFL and updateDL
updateAndPropagate                      :: (fl -> dl -> dl) ->
                                           (dl -> dl -> dl) ->
                                           (FSTree fl dl -> FSTree fl dl) ->
                                           FSTreeZipper fl dl zs -> FSTreeZipper fl dl zs
updateAndPropagate updateFL updateDL f z = case update f z of
  (t, bs, zs) -> let -- updateL is a function that given a label, computes the new
                     -- label value.
                     updateL = case t of
                         F t' -> updateFL (fileLabel t')
                         D t' -> updateDL (dirLabel t')
                 in ( t
                      -- We traverse all crumbs, left to right, and incrementally update
                      -- all crumbs with their new label. If we have computed a new
                      -- crumb c, we use it's label in the propagation function in
                      -- the next step.
                    , reverse . snd $ foldl (\(updateF,bs') c ->
                                              let c' = updateFSCrumb c updateF in
                                              (updateDL (label' c'), c':bs')
                                  ) (updateL, []) bs
                    , zs)


updateFSCrumb              :: FSCrumb fl dl -> (dl -> dl) -> FSCrumb fl dl
updateFSCrumb (FCrumb c) f = FCrumb $ updateCrumb c f
updateFSCrumb (DCrumb c) f = DCrumb $ updateCrumb c f

updateCrumb     :: Crumb dl unCh ch -> (dl -> dl) -> Crumb dl unCh ch
updateCrumb c f = c { dirLabel' = f (dirLabel' c) }


-- | Replace the currently selected subtree
replace   :: FSTree fl dl -> FSTreeZipper fl dl zs -> FSTreeZipper fl dl zs
replace t = update (const t)


withTree             :: (FSTree fl dl -> FSTree fl dl) ->
                        FSTreeZipper fl dl zs -> FSTreeZipper fl dl zs
withTree f (t,bs,zs) = (f t, bs, zs)


-- | Delete the currently selected tree. This moves us up to the parent node of
-- the zipper.
delete               :: FSTreeZipper fl dl zs -> Maybe (FSTreeZipper fl dl zs)
delete z@(F f, _, _) = update (\(D d') -> D $ removeFile   (fileName f) d') <$> goUp z
delete z@(D d, _, _) = update (\(D d') -> D $ removeSubDir (dirName d)  d') <$> goUp z
