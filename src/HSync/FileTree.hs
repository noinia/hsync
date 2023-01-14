{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.FileTree
  ( FileTreeRoot(Root), rootPath, tree
  , mapUpFileTreeRoot, mapUpFileTreeRootM

  , FileTree(..)
  , DirectoryContent
  , FileName
  , mapUpFileTree, mapUpFileTreeM


  , DirAttrs(..), cached, dirAttr

  , readFileTreeWithLastModified
  , readFileTree
  , readFileTreeWith


  ) where

import           Control.Lens
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Coerce
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Semigroup (Max(..))
import           Data.Time.Clock
import           Flat
import           System.Directory.OsPath
import           System.OsPath

import           System.OsPath.Posix (PosixString)
import           System.OsPath.Windows (WindowsString)


--------------------------------------------------------------------------------

-- | File names
type FileName = OsString

instance Flat OsString
instance Flat PosixString
instance Flat WindowsString


-- | A rooted file tree.
data FileTreeRoot d f = Root { _rootPath :: !OsPath
                             , _tree     :: !(FileTree d f)
                             }
                      deriving stock (Show,Eq,Generic,Functor,Foldable,Traversable)

instance (Flat d, Flat f) => Flat (FileTreeRoot d f)

instance Bifunctor FileTreeRoot where
  bimap d f = over tree (bimap d f)
instance Bifoldable FileTreeRoot where
  bifoldMap d f = foldMapOf tree (bifoldMap d f)
instance Bitraversable FileTreeRoot where
  bitraverse d f = traverseOf tree (bitraverse d f)

rootPath :: Lens' (FileTreeRoot d f) OsPath
rootPath = lens _rootPath (\(Root _ t) r -> Root r t)

tree :: Lens (FileTreeRoot d f) (FileTreeRoot e b) (FileTree d f) (FileTree e b)
tree = lens _tree (\(Root r _) t -> Root r t)


-- | the parent already stores the name of this file
data FileTree d f = File      !f
                  | Directory !d (DirectoryContent d f)
                  deriving (Show,Eq,Generic,Functor,Foldable,Traversable)

type DirectoryContent d f = Map.Map FileName (FileTree d f)

instance (Flat d, Flat f) => Flat (FileTree d f)


--------------------------------------------------------------------------------

instance Bifunctor FileTree where
  bimap d f = \case
    File x          -> File $ f x
    Directory x chs -> Directory (d x) $ Map.map (bimap d f) chs

instance Bifoldable FileTree where
  bifoldMap d f = \case
    File x          -> f x
    Directory x chs -> d x <> Map.foldMapWithKey (\_ c -> bifoldMap d f c) chs

instance Bitraversable FileTree where
  bitraverse d f = \case
    File x          -> File <$> f x
    Directory x chs -> Directory <$> d x <*> Map.traverseWithKey (\_ -> bitraverse d f) chs

--------------------------------------------------------------------------------

-- | directory attributes that supports local attributes as well as a cache.
data DirAttrs cache d = DirAttrs !cache -- ^ cached attributes about the content
                                 !d  -- ^ "local" attributes about this directory itself
                      deriving stock (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)

cached :: Lens (DirAttrs cache d) (DirAttrs cache' d) cache cache'
cached = lens (\(DirAttrs c _) -> c) (\(DirAttrs _ d) c -> DirAttrs c d)

dirAttr :: Lens (DirAttrs cache d) (DirAttrs cache d') d d'
dirAttr = lens (\(DirAttrs _ d) -> d) (\(DirAttrs c _) d -> DirAttrs c d)

instance Bifunctor DirAttrs where
  bimap f g (DirAttrs c d) = DirAttrs (f c) (g d)
instance Bifoldable DirAttrs where
  bifoldMap f g (DirAttrs c d) = f c <> g d
instance Bitraversable DirAttrs where
  bitraverse f g (DirAttrs c d) = DirAttrs <$> f c <*> g d


mapUpFileTreeRootM       :: (Monad m, Monoid cache)
                         => (d -> cache -> m (d',cache) )
                         -> (f -> m (f', cache) )
                         -> FileTreeRoot d f
                         -> m (FileTreeRoot (DirAttrs cache d') f')
mapUpFileTreeRootM d f t = t&tree %%~ mapUpFileTreeM d f

mapUpFileTreeRoot     :: Monoid cache
                      => (d -> cache -> (d',cache) )
                      -> (f -> (f', cache) )
                      -> FileTreeRoot d f
                      -> FileTreeRoot (DirAttrs cache d') f'
mapUpFileTreeRoot d f = over tree (mapUpFileTree d f)

mapUpFileTreeM     :: (Monad m, Monoid cache)
                   => (d -> cache -> m (d',cache) )
                   -> (f -> m (f', cache) )
                   -> FileTree d f
                   -> m (FileTree (DirAttrs cache d') f')
mapUpFileTreeM d f = fmap fst . go
  where
    go = \case
      File x          -> over _1 File <$> f x
      Directory x chs -> do chs'      <- Map.traverseWithKey (const go) chs
                            (y,cache) <- d x $ foldMap snd chs'
                            pure (Directory (DirAttrs cache y) (fmap fst chs'), cache)


-- | compute caches in an upward way
mapUpFileTree      :: (Monoid cache)
                   => (d -> cache -> (d',cache) )
                   -> (f -> (f', cache) )
                   -> FileTree d f
                   -> FileTree (DirAttrs cache d') f'
mapUpFileTree d f = runIdentity . mapUpFileTreeM (\a b -> Identity $ d a b) (Identity . f)


--------------------------------------------------------------------------------

newtype ModificationTime = ModificationTime UTCTime
  deriving stock (Show,Eq,Ord)
  deriving Semigroup via Max UTCTime

instance Monoid ModificationTime where
  mempty = ModificationTime $ UTCTime (toEnum 0) 0


-- | Read the file with last modification times.
readFileTreeWithLastModified :: OsPath
                             -> IO (FileTreeRoot (DirAttrs ModificationTime ModificationTime)
                                                 ModificationTime
                                   )
readFileTreeWithLastModified = fmap (mapUpFileTreeRoot (\t acc -> (t, t <> acc))
                                                       (\x -> (x,x)))
                             . readFileTreeWith getModificationTime' getModificationTime'
  where
    getModificationTime' = fmap ModificationTime <$> getModificationTime


-- | reads a directory into a rooted File tree
readFileTree :: OsPath -> IO (FileTreeRoot () ())
readFileTree = readFileTreeWith (const $ pure ()) (const $ pure ())

-- | reads a directory into a rooted File tree
readFileTreeWith          :: (OsPath -> IO d) -- ^ to compute the dir info
                          -> (OsPath -> IO f) -- ^ to compute the file info
                          -> OsPath -- root hatp h
                          -> IO (FileTreeRoot d f)
readFileTreeWith d f path = Root path <$> readFileTree' d f path

-- | The function that does the actual work reading the fileTree
readFileTree'           :: (OsPath -> IO d) -- ^ to compute the dir info
                        -> (OsPath -> IO f) -- ^ to compute the file info
                        -> OsPath -> IO (FileTree d f)
readFileTree' getD getF = go
  where
    go path = do d <- getD path
                 content <- listDirectory path
                 (dirs,files) <- partitionM (doesDirectoryExist . rooted) content
                 files' <- mapM (\n -> (\f -> (n,File f)) <$> getF (rooted n)) files
                 dirs'  <- mapM (\n -> (\t -> (n,t))      <$> go   (rooted n)) dirs
                 pure $ Directory d (Map.fromList $ files' <> dirs')
      where
        rooted = (path </>)

-- | Monadic version of partition
partitionM   :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p = fmap ( bimap (map fst) (map fst)
                    . List.partition snd)
             . mapM (\x -> (x,) <$> p x)
