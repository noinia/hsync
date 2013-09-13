module HSync.Client.FSStatus( FSStatus(..)
                            , fsStatus
                            ) where


import Data.Default

import HSync.Common.FSTree
import HSync.Client.MergeTree


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
