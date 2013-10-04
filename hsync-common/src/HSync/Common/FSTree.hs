{-# Language TemplateHaskell #-}
module HSync.Common.FSTree( FSTree(..)
                          , nonEmpty
                          , getNonEmptyTree
                          , toMaybe


                          , FSTree'(..)


                          , name
                          , isRegularFile
                          , isDirectory
                          , label
                          , children
                          , children'


                          , childrenWithNames
                          , withNames

                          , gmap
                          , gmap'

                          , subTree
                          , findChild
                          , findChild'

                          , labelWithSubPaths

                          , readFSTree'


                          , readFSTree
                          , toFileIdent
                          , toFileIdent'

                          , sequenceBottomUp
                          , runBottomUp

                          ) where


import Prelude hiding (foldl,foldr)

import Control.Arrow((&&&))
import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO, MonadIO(..))

import Data.Aeson.TH

import Data.List(isPrefixOf)

import Data.Maybe(listToMaybe , mapMaybe )
import Data.Monoid
import Data.Foldable hiding (elem)

import Data.Text(Text)

import HSync.Common.AtomicIO
import HSync.Common.DateTime(DateTime, modificationTime)
import HSync.Common.Types(FileName, SubPath)


import System.Directory
import System.FilePath (takeFileName, dropTrailingPathSeparator, (</>))
import System.Directory (getDirectoryContents)

import qualified Data.Text              as T
import qualified HSync.Common.FileIdent as FI
import qualified Data.Map               as M
--------------------------------------------------------------------------------

data FSTree l = NoFiles
              | FSTree (FSTree' l)
              deriving (Show,Read,Eq)

-- | analogue of maybe
nonEmpty                :: b -> (FSTree' l -> b) -> FSTree l -> b
nonEmpty z _ NoFiles    = z
nonEmpty _ f (FSTree t) = f t


toMaybe :: FSTree l -> Maybe (FSTree' l)
toMaybe = nonEmpty Nothing Just

getNonEmptyTree            :: FSTree l -> FSTree' l
getNonEmptyTree (FSTree t) = t


data FSTree' l = Directory FileName l [FSTree' l]
               | File      FileName l
                deriving (Show,Read,Eq)

instance Functor FSTree where
    fmap f = gmap f f

instance Functor FSTree' where
    fmap f = gmap' f f


-- | A slightly more generic fmap: gmap dirF fileF t runs dirF on all
-- directories, and fileF on all files
gmap       :: (l -> l') -> (l -> l') -> FSTree l -> FSTree l'
gmap df ff = nonEmpty NoFiles (FSTree . gmap' df ff)

gmap'                           :: (l -> l') -> (l -> l') ->
                                   FSTree' l -> FSTree' l'
gmap' df ff (Directory n l chs) = Directory n (df l) $ map (gmap' df ff) chs
gmap' _  ff (File      n l)     = File      n (ff l)

instance Foldable FSTree where
    foldMap f = nonEmpty mempty (foldMap f)

instance Foldable FSTree' where
    foldMap f (Directory _ l chs) = f l <> foldMap (foldMap f) chs
    foldMap f (File      _ l)     = f l

$(deriveJSON id ''FSTree')
$(deriveJSON id ''FSTree)

name                   :: FSTree' l -> FileName
name (Directory n _ _) = n
name (File      n _)   = n


isRegularFile            :: FSTree' l -> Bool
isRegularFile (File _ _) = True
isRegularFile _          = False

isDirectory                   :: FSTree' l -> Bool
isDirectory (Directory _ _ _) = True
isDirectory _                 = False


label                   :: FSTree' l -> l
label (Directory _ l _) = l
label (File      _ l)   = l



children :: FSTree l -> [FSTree' l]
children = nonEmpty [] children'

children'                     :: FSTree' l -> [FSTree' l]
children' (Directory _ _ chs) = chs
children' _                   = []


childrenWithNames = nonEmpty mempty childrenWithNames'

childrenWithNames' :: FSTree' l -> M.Map FileName (FSTree' l)
childrenWithNames' = withNames . children'

-- | Given a list of FSTree's, create a map with their names as keys, and the
-- subtrees themselves as the values
withNames :: [FSTree' l] -> M.Map FileName (FSTree' l)
withNames = M.fromList . map (name &&& id)


subTree    :: SubPath -> FSTree l -> Maybe (FSTree l)
subTree sp = nonEmpty Nothing (fmap FSTree . subTree' sp)


-- | Given a tree and a subPath, get the filetree rooted at
-- the node specified by the subPath
subTree'      :: SubPath -> FSTree' l -> Maybe (FSTree' l)
subTree' sp t = foldl (\mt n -> mt >>= findChild' n) (Just t) $ sp


-- | Given a name n and a FSTree v, get the child of v with name n (if it
-- exists)
findChild   :: FileName -> FSTree l -> Maybe (FSTree' l)
findChild n = nonEmpty Nothing (findChild' n)

-- | Given a name n and a FSTree' v, get the child of v with name n (if it
-- exists)
findChild'   :: FileName -> FSTree' l -> Maybe (FSTree' l)
findChild' n = listToMaybe . filter ((n ==) . name) . children'


-- | Add the full subpath in the label of each node
-- (i.e. the path from the root of the tree to the current node)
labelWithSubPaths :: FSTree l -> FSTree (l, SubPath)
labelWithSubPaths = nonEmpty NoFiles (FSTree . labelWithSubPaths')

-- | Same as above, but on a FSTree'
labelWithSubPaths'  :: FSTree' l -> FSTree' (l,SubPath)
labelWithSubPaths' = labelWithSubPaths'' []
    where
      labelWithSubPaths'' ps (File      n l)     = File n (l,mkSP n ps)
      labelWithSubPaths'' ps (Directory n l chs) =
          Directory n (l,mkSP n ps) $ map (labelWithSubPaths'' (n:ps)) chs
      mkSP n ps = reverse (n:ps)



-- | Read an FSTree' from the disk. Use the genLabel function to generate the
-- labels stored in each node. This function assumes that the file-path exists
-- on disk if not, the function throws an exception.
readFSTree'            :: (Functor m, MonadIO m) =>
                         (FilePath -> m l) ->  FilePath -> m (FSTree l)
readFSTree' genLabel p = do
                          t <- exists p
                          l <- genLabel p
                          case t of
                            (False, False) -> return   NoFiles
                            (True,  False) -> return . FSTree $ File      n l
                            (_,     True)  ->         (FSTree . Directory n l) <$> chs
                            -- (True,  True)  ->
                            --     error "readFSTree: p is both a file and a directory"
    where
      n            = T.pack . takeFileName . dropTrailingPathSeparator $ p
      selfOrParent = (`elem` [".",".."])
      chs          = do
                       ps'  <- liftIO $ getDirectoryContents p
                       let ps = map (p </>) . filter (not . selfOrParent) $ ps'
                       chs' <- mapM (readFSTree' genLabel) ps
                       return . mapMaybe toMaybe $ chs'

--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- | Running actions on FSTrees

-- | Runs all actions in the tree, bottom up
sequenceBottomUp                       :: Monad m => FSTree' (m a) -> m (FSTree' a)
sequenceBottomUp (File n act)          = act >>= return . File n
sequenceBottomUp (Directory n act chs) = do
                                           chs' <- mapM sequenceBottomUp chs
                                           x    <- act
                                           return $ Directory n x chs'

-- | runs all functions in the tree bottom up
runBottomUp   :: (Functor m, Monad m) => (l -> m a) -> FSTree l -> m (FSTree a)
runBottomUp f = nonEmpty (return NoFiles) (fmap FSTree . runBottomUp' f)

runBottomUp'   :: Monad m => (l -> m a) -> FSTree' l -> m (FSTree' a)
runBottomUp' f = sequenceBottomUp . fmap f

--------------------------------------------------------------------------------


readFSTree :: (Functor m, MonadIO m) => FilePath -> m (FSTree DateTime)
readFSTree = readFSTree' modificationTime


toFileIdent :: FSTree DateTime -> FI.FileIdent
toFileIdent = nonEmpty FI.NonExistent toFileIdent'

toFileIdent'                   :: FSTree' DateTime -> FI.FileIdent
toFileIdent' (Directory _ d _) = FI.Directory d
toFileIdent' (File      _ d)   = FI.File d


--------------------------------------------------------------------------------
-- | Testing stuff
