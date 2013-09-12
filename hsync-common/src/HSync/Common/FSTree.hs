{-# Language TemplateHaskell #-}
module HSync.Common.FSTree( FSTree(..)
                          , Name
                          , name
                          , isRegularFile
                          , isDirectory
                          , label
                          , children

                          , readFSTree'


                          , readFSTree
                          , toFileIdent
                          , toFileIdent'


                          -- , Diff(..)
                          -- , diffTrees

                          , sequenceBottomUp
                          , runBottomUp

                          ) where


import Control.Arrow((&&&))
import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO, MonadIO(..))
import Control.Monad((>=>))

import Data.Aeson.TH

import Data.Foldable hiding (elem)
import Data.Monoid((<>))
import Data.Maybe(mapMaybe)

import Data.List(isPrefixOf)
import Data.Text(Text)
import Data.Tree

import HSync.Common.AtomicIO
import HSync.Common.DateTime(DateTime, modificationTime)



import System.Directory
import System.FilePath (takeFileName, dropTrailingPathSeparator, (</>))
import System.Directory (getDirectoryContents)

import qualified Data.Text              as T
import qualified HSync.Common.FileIdent as FI
import qualified Data.Map               as M

--------------------------------------------------------------------------------

type Name = Text

data FSTree l = Directory Name l [FSTree l]
              | File      Name l
                deriving (Show,Read,Eq)


instance Functor FSTree where
    fmap f (Directory n l chs) = Directory n (f l) $ map (fmap f) chs
    fmap f (File      n l)     = File      n (f l)


instance Foldable FSTree where
    foldMap f (Directory _ l chs) = f l <> foldMap (foldMap f) chs
    foldMap f (File      _ l)     = f l

$(deriveJSON id ''FSTree)


name                   :: FSTree l -> Name
name (Directory n _ _) = n
name (File      n _)   = n


isRegularFile            :: FSTree l -> Bool
isRegularFile (File _ _) = True
isRegularFile _          = False

isDirectory                   :: FSTree l -> Bool
isDirectory (Directory _ _ _) = True
isDirectory _                 = False

label                   :: FSTree l -> l
label (Directory _ l _) = l
label (File      _ l)   = l


children                     :: FSTree l -> [FSTree l]
children (Directory _ _ chs) = chs
children _                   = []


childrenWithNames :: FSTree l -> M.Map Name (FSTree l)
childrenWithNames = withNames . children


withNames = M.fromList . map (name &&& id)



-- | Read an FSTree from the disk. Use the genLabel function to generate the
-- labels stored in each node. This function assumes that the file-path exists
-- on disk if not, the function throws an exception.
readFSTree'            :: (Functor m, MonadIO m) =>
                         (FilePath -> m l) ->  FilePath -> m (FSTree l)
readFSTree' genLabel p = do
                          t <- exists p
                          l <- genLabel p
                          case t of
                            (False, False) ->
                                error "readFSTree: file is no file or directory?"
                            (True,  False) -> return $ File      n l
                            (_,     True)  ->          Directory n l <$> chs
                            -- (True,  True)  ->
                            --     error "readFSTree: p is both a file and a directory"
    where
      n            = T.pack . takeFileName . dropTrailingPathSeparator $ p
      selfOrParent = (`elem` [".",".."])
      chs          = do
                       ps' <- liftIO $ getDirectoryContents p
                       let ps = map (p </>) . filter (not . selfOrParent) $ ps'
                       mapM (readFSTree' genLabel) ps



--------------------------------------------------------------------------------




data FSType = F | D
            deriving (Show,Eq,Read)


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

-- | Assumes the names of the initial trees are identical so it only stores the left name
mergeTrees                                             :: FSTree l -> FSTree r ->
                                                          Tree (Merge l r)
mergeTrees (File n ll)           (File      _ rl)      = merge n ll F []   rl F []   []
mergeTrees (File n ll)           (Directory _ rl rchs) = merge n ll F []   rl D rchs []
mergeTrees (Directory n ll lchs) (File      _ rl)      = merge n ll D lchs rl F []   []
mergeTrees (Directory n ll lchs) (Directory _ rl rchs) = merge n ll D lch' rl D rch' chs'
    where
      lch'    = fromMap $ M.difference lm rm
      rch'    = fromMap $ M.difference rm lm
      chs'    = fromMap $ M.intersectionWith mergeTrees lm rm
      fromMap = map snd . M.toList
      lm      = withNames lchs
      rm      = withNames rchs


-- | extracts the left tree out of a Tree (Merge l r). Note that
-- leftTree (mergeTrees l r) should have the same elements as l, but not
-- neccesarily in the same order.
leftTree                          :: Tree (Merge l r) -> FSTree l
leftTree (Node (Merge n l _) chs) = fromItemData n (map leftTree chs) l

rightTree                          :: Tree (Merge l r) -> FSTree r
rightTree (Node (Merge n _ r) chs) = fromItemData n (map rightTree chs) r




-- | Adapt the tree using function f in a bottom up fashion. If f n returns Nothing
-- we remove the node from the tree. If f n returns Just n' we replace n by n'. Note
-- that we run this function bottom up. So we replace the children of n *before*
-- running f n
adaptBottomUp                 :: (Tree a -> Maybe (Tree a)) ->
                                 Tree a -> Maybe (Tree a)
adaptBottomUp f (Node l chs) = let chs' = mapMaybe (adaptBottomUp f) chs in
                               f $ Node l chs'

-- | Filter the tree in a bottom up fashion. So we filter the children of a node n
-- *before* we apply predicate p on node n itself.
--
-- Note that when the children of n satisfy p, but n itself does not, we still remove
-- the node (together with all its children) from the tree. If this is undesirable,
-- use `filterNonEmpty`.
filterBottomUp   :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterBottomUp p = adaptBottomUp (\n -> if p n then Just n else Nothing)

-- | Filters the tree bottom up in the same way as `filterBottomUp`. However, if a node
-- n has children that satisfy the predicate, we will keep n (with only the children that
-- satisfy p), even though n itself may not satisfy the predicate.
filterNonEmpty   :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterNonEmpty p = filterBottomUp (\n -> p n || hasChildren n)
                   where
                     hasChildren = not . null . subForest

items (Merge _ l r) = (l,r)

labels m = let (l,r) = items m in (label' l, label' r)

-- | Filter the merge tree, retaining only those nodes (and their ancestors) whose
-- left label is smaller than their right label.
smallerInLeft' :: Ord l => Tree (Merge l l) -> Maybe (Tree (Merge l l))
smallerInLeft' = filterNonEmpty (uncurry (<) . labels . rootLabel)


smallerInLeft l r = smallerInLeft' $ mergeTrees l r


-- Find everyting that is in only left, and not in right. Note that if both lt and rt
-- have an element, but l's label is newer than r's. This is *NOT* reported.
newInLeft     :: FSTree l -> FSTree r -> Maybe (FSTree l)
newInLeft l r = projectLeftWith newInLeft' l r

newInLeft' :: Tree (Merge l r) -> Maybe (Tree (Merge l r))
newInLeft' = filterNonEmpty (not . null . uniqueChildren' . left . rootLabel)


-- | Throws away all subtrees that only occur in the left subtree
ignoreOnlyLeft' = adaptBottomUp (Just . withRootLabel f)
    where
      f      = updateMerge g id
      g item = item { uniqueChildren' = [] }




-- | Everything that occurs in *both* subtrees, but is newer in the left subtree
newerInLeft' :: Ord l => Tree (Merge l l) -> Maybe (Tree (Merge l l))
newerInLeft' = filterNonEmpty (uncurry (>) . labels . rootLabel) >=> ignoreOnlyLeft'


newerInLeft l r = projectLeftWith newerInLeft' l r


-- | Shorthand to merge to the two trees, run f on the result, and then project
--  the left subtree
projectLeftWith       :: Functor f => (Tree (Merge l r) -> f (Tree (Merge l' r'))) ->
                         FSTree l -> FSTree r -> f (FSTree l')
projectLeftWith f l r = fmap leftTree . f $ mergeTrees l r



-- | updateMerge f g m  applies f on the left item and g on the right item
updateMerge                   :: (FSItemData l -> FSItemData l')  ->
                                 (FSItemData r -> FSItemData r') ->
                                 Merge l r -> Merge l' r'
updateMerge f g (Merge n l r) = Merge n (f l) (g r)


-- | Applies a function on the root label
withRootLabel                :: (a -> a) -> Tree a -> Tree a
withRootLabel f (Node m chs) = Node (f m) chs






-- toDownload'   :: Tree (Merge l l) -> Maybe (Tree (Merge l l))
-- toDownload' t = filterNonEmpty p
--     where
--       p n =


--------------------------------------------------------------------------------
-- | Running actions on FSTrees

-- | Runs all actions in the tree, bottom up
sequenceBottomUp                       :: Monad m => FSTree (m a) -> m (FSTree a)
sequenceBottomUp (File n act)          = act >>= return . File n
sequenceBottomUp (Directory n act chs) = do
                                           chs' <- mapM sequenceBottomUp chs
                                           x    <- act
                                           return $ Directory n x chs'

-- | runs all functions in the tree bottom up
runBottomUp   :: Monad m => (l -> m a) -> FSTree l -> m (FSTree a)
runBottomUp f = sequenceBottomUp . fmap f

--------------------------------------------------------------------------------


readFSTree :: (Functor m, MonadIO m) => FilePath -> m (FSTree DateTime)
readFSTree = readFSTree' modificationTime


toFileIdent :: Maybe (FSTree DateTime) -> FI.FileIdent
toFileIdent = maybe FI.NonExistent toFileIdent'

toFileIdent'                   :: FSTree DateTime -> FI.FileIdent
toFileIdent' (Directory _ d _) = FI.Directory d
toFileIdent' (File      _ d)   = FI.File d


--------------------------------------------------------------------------------
-- | Testing stuff

leftT = Directory "root" 0 [ File "onlyLeft"    1
                           , File "both"        2
                           , Directory "subdir" 3 [ File "foo" 4
                                                  ]
                           ]

rightT = Directory "root" 0 [ File "onlyRight"   10
                            , File "both"        2
                            , Directory "subdir" 3 [ File "foo" 5
                                                   , File "bar" 6
                                                   ]
                            ]
