module HSync.Common.MergeTree( MergeTree
                             , mergeTree
                             , leftTree
                             , rightTree

                             , FSItemData(..)
                             , FSType(..)
                             , Merge(..)

                             , projectLeftWith

                             , adjustBottomUp
                             , filterBottomUp
                             , filterNonEmpty

                             , newInLeft
                             , newInRight

                             , newerInLeft
                             , ignoreOnlies
                             , ignoreOnlyLeft

                             , typeChanged

                             , FSStatus(..)
                             , fsStatus

                             ) where

import Data.Monoid((<>))
import Data.Maybe(mapMaybe)
import Data.Tree


import HSync.Common.FSTree

import Control.Monad((>=>),(<=<))


import qualified Data.Map               as M

--------------------------------------------------------------------------------

-- | We distinguish between files and directories
data FSType = F | D
            deriving (Show,Eq,Read)

-- | The information/data corresponding to a single item in the filesystem
data FSItemData l = Item { label'          :: l
                         , type'           :: FSType
                         , uniqueChildren' :: [FSTree l]
                         }
                    deriving (Show,Eq,Read)

-- | Given a name, a list of extra children and a FSItemData, construct an FSTree
fromItemData                       :: Name -> [FSTree l] -> FSItemData l -> FSTree l
fromItemData n []  (Item l F [])   = File n l
fromItemData n _   (Item _ F _)    = error "fromItemData: Files cannot have children!"
fromItemData n chs (Item l D chs') = Directory n l (chs <> chs')


--------------------------------------------------------------------------------

-- | Data type to hold the information of two files
data Merge l r = Merge { name'         :: Name
                       , left          :: FSItemData l
                       , right         :: FSItemData r
                       }
                 deriving (Eq,Show,Read)

-- | Construct a merge node
merge                             :: Name
                                  -> l -> FSType -> [FSTree l]
                                  -> r -> FSType -> [FSTree r]
                                  -> Forest (Merge l r)
                                  -> Tree (Merge l r)
merge n ll lt lchs rl rt rchs chs = Node (Merge n
                                                (Item ll lt lchs)
                                                (Item rl rt rchs)
                                         ) chs

-- | Given a merge node, collect the FSItem data of both the left and the right file
gather                 :: (FSItemData a -> b) -> Merge a a -> (b, b)
gather f (Merge _ l r) = (f l,f r)


swapMerge               :: Merge r l -> Merge l r
swapMerge (Merge n l r) = Merge n r l


-- | updateMerge f g m  applies f on the left item and g on the right item
updateMerge                   :: (FSItemData l -> FSItemData l')  ->
                                 (FSItemData r -> FSItemData r') ->
                                 Merge l r -> Merge l' r'
updateMerge f g (Merge n l r) = Merge n (f l) (g r)

--------------------------------------------------------------------------------

-- | Represents two related FSTrees
type MergeTree l r = Tree (Merge l r)


-- | Assumes the names of the initial trees are identical so it only stores the left name
mergeTree                                             :: FSTree l -> FSTree r ->
                                                          MergeTree l r
mergeTree (File n ll)           (File      _ rl)      = merge n ll F []   rl F []   []
mergeTree (File n ll)           (Directory _ rl rchs) = merge n ll F []   rl D rchs []
mergeTree (Directory n ll lchs) (File      _ rl)      = merge n ll D lchs rl F []   []
mergeTree (Directory n ll lchs) (Directory _ rl rchs) = merge n ll D lch' rl D rch' chs'
    where
      lch'    = fromMap $ M.difference lm rm
      rch'    = fromMap $ M.difference rm lm
      chs'    = fromMap $ M.intersectionWith mergeTree lm rm
      fromMap = map snd . M.toList
      lm      = withNames lchs
      rm      = withNames rchs


-- | extracts the left tree out of a MergeTree l r. Note that
-- leftTree (mergeTree l r) should have the same elements as l, but not
-- neccesarily in the same order.
leftTree                          :: MergeTree l r -> FSTree l
leftTree (Node (Merge n l _) chs) = fromItemData n (map leftTree chs) l

-- | Extracts the right tree out of MergeTree l r. See 'LeftTree' for more
-- details.
rightTree                          :: MergeTree l r -> FSTree r
rightTree (Node (Merge n _ r) chs) = fromItemData n (map rightTree chs) r


-- | Throws away all subtrees that only occur in the left subtree
ignoreOnlyLeft :: MergeTree l r -> Maybe (MergeTree l r)
ignoreOnlyLeft = adjustBottomUp (Just . withRootLabel f)
    where
      f      = updateMerge g id
      g item = item { uniqueChildren' = [] }

-- | Throws away all subtrees that occur in only one tree
ignoreOnlies :: MergeTree l r -> Maybe (MergeTree l r)
ignoreOnlies = ignoreOnlyLeft
               >=> return . fmap swapMerge
               >=> ignoreOnlyLeft >=> return . fmap swapMerge


-- | Adjust the tree using function f in a bottom up fashion. If f n returns Nothing
-- we remove the node from the tree. If f n returns Just n' we replace n by n'. Note
-- that we run this function bottom up. So we replace the children of n *before*
-- running f n
adjustBottomUp                 :: (Tree a -> Maybe (Tree a)) ->
                                 Tree a -> Maybe (Tree a)
adjustBottomUp f (Node l chs) = let chs' = mapMaybe (adjustBottomUp f) chs in
                               f $ Node l chs'

-- | Filter the tree in a bottom up fashion. So we filter the children of a node n
-- *before* we apply predicate p on node n itself.
--
-- Note that when the children of n satisfy p, but n itself does not, we still remove
-- the node (together with all its children) from the tree. If this is undesirable,
-- use `filterNonEmpty`.
filterBottomUp   :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterBottomUp p = adjustBottomUp (\n -> if p n then Just n else Nothing)

-- | Filters the tree bottom up in the same way as `filterBottomUp`. However, if a node
-- n has children that satisfy the predicate, we will keep n (with only the children that
-- satisfy p), even though n itself may not satisfy the predicate.
filterNonEmpty   :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterNonEmpty p = filterBottomUp (\n -> p n || hasChildren n)
                   where
                     hasChildren = not . null . subForest

-- | Filter the merge tree, retaining only those nodes (and their ancestors) whose
-- left label is smaller than their right label
smallerInLeft :: Ord l => MergeTree l l -> Maybe (MergeTree l l)
smallerInLeft = filterNonEmpty (uncurry (<) . gather label' . rootLabel)


-- Find everyting that is in only left, and not in right. Note that if both lt and rt
-- have an element, but l's label is newer than r's. This is *NOT* reported.
newInLeft :: MergeTree l r -> Maybe (MergeTree l r)
newInLeft = filterNonEmpty (not . null . uniqueChildren' . left . rootLabel)

-- | Symmetric to 'newInLeft'
newInRight :: MergeTree l r -> Maybe (MergeTree l r)
newInRight = fmap (fmap swapMerge) . newInLeft . fmap swapMerge

-- | Extract everything that occurs in *both* subtrees, but is newer in the
-- left subtree.  Note that this removes all trees that occur in only a single
-- tree.
newerInLeft :: Ord l => MergeTree l l -> Maybe (MergeTree l l)
newerInLeft = ignoreOnlies >=> filterNonEmpty (uncurry (>) . gather label' . rootLabel)

-- | Report those items whose type has changed. Note that this removes all
-- subtrees that occur in only one tree.
typeChanged :: MergeTree l l -> Maybe (MergeTree l l)
typeChanged = ignoreOnlies >=> filterNonEmpty (uncurry (/=) . gather type' . rootLabel)
-- FIXME: If the type has changed, then one of the types is a directory, which is the
-- only item of the two that has children. We throw away this information by the
-- ignoreOnlies'

-- | Shorthand to merge to the two trees, run f on the result, and then project
--  the left subtree
projectLeftWith       :: Functor c => (MergeTree l r -> c (MergeTree l' r')) ->
                         FSTree l -> FSTree r -> c (FSTree l')
projectLeftWith f l r = fmap leftTree . f $ mergeTree l r

--------------------------------------------------------------------------------
-- | A data type to represent the status of the file system. I..e which files
-- have been added, deleted, and which files have been updated.


data FSStatus l = FSStatus { added        :: Maybe (FSTree l)
                           , deleted      :: Maybe (FSTree l)
                           , updated      :: Maybe (MergeTree l l)
                           }
                  deriving (Show,Read,Eq)


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


--------------------------------------------------------------------------------
-- | A data type representing conflicts

-- conflicts :: FSTree l l -> FSTree l l -> FSTree l l -> FSStatus l
-- conflicts oldRemote newLocal newRemote =
--     where
--       localFSStatus   = fsStatus oldRemote newLocal
--       remoteFSStatuss = fsStatus oldRemote newRemote

--------------------------------------------------------------------------------
-- | Helper functions

-- | Applies a function on the root label
withRootLabel                :: (a -> a) -> Tree a -> Tree a
withRootLabel f (Node m chs) = Node (f m) chs

--------------------------------------------------------------------------------
-- | Testing stuff

leftT = Directory "root" 0 [ File "onlyLeft"    1
                           , File "both"        2
                           , Directory "subdir" 3 [ File "foo" 4
                                                  ]
                           , Directory "baz" 100 []
                           ]

rightT = Directory "root" 0 [ File "onlyRight"   10
                            , File "both"        2
                            , Directory "subdir" 3 [ File "foo" 5
                                                   , File "bar" 6
                                                   ]
                            , File "baz" 101
                            ]
