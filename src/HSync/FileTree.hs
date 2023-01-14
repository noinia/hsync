{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSync.FileTree
  ( FileTreeRoot(Root), rootPath, tree
  , FileTree(..)
  , DirectoryContent
  , FileName

  , readFileTreeWithLastModified
  , readFileTree
  , readFileTreeWith
  ) where

import           Control.Lens
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Time.Clock
import           Flat
import           System.Directory.OsPath
import           System.OsPath


import           System.OsPath.Posix (PosixString)
import           System.OsPath.Windows (WindowsString)


--------------------------------------------------------------------------------

type FileName = OsString

instance Flat OsString
instance Flat PosixString
instance Flat WindowsString


-- | A rooted file tree.
data FileTreeRoot d f = Root { _rootPath :: !OsPath
                             , _tree     :: !(FileTree d f)
                             }
                      deriving stock (Show,Eq,Generic)

instance (Flat d, Flat f) => Flat (FileTreeRoot d f)

rootPath :: Lens' (FileTreeRoot d f) OsPath
rootPath = lens _rootPath (\(Root _ t) r -> Root r t)

tree :: Lens (FileTreeRoot d f) (FileTreeRoot e b) (FileTree d f) (FileTree e b)
tree = lens _tree (\(Root r _) t -> Root r t)


-- | the parent already stores the name of this file
data FileTree d f = File      !f
                  | Directory !d (DirectoryContent d f)
                  deriving (Show,Eq,Generic)

type DirectoryContent d f = Map.Map FileName (FileTree d f)

instance (Flat d, Flat f) => Flat (FileTree d f)


--------------------------------------------------------------------------------

-- | Read the file with last modification times.
readFileTreeWithLastModified :: OsPath -> IO (FileTreeRoot UTCTime UTCTime)
readFileTreeWithLastModified = readFileTreeWith getModificationTime getModificationTime

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
