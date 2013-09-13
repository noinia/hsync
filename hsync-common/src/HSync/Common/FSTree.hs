{-# Language TemplateHaskell #-}
module HSync.Common.FSTree( FSTree(..)
                          , name
                          , isRegularFile
                          , isDirectory
                          , label
                          , children

                          , childrenWithNames
                          , withNames

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

import Data.Maybe(listToMaybe)
import Data.Monoid((<>))
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

data FSTree l = Directory FileName l [FSTree l]
              | File      FileName l
                deriving (Show,Read,Eq)


instance Functor FSTree where
    fmap f (Directory n l chs) = Directory n (f l) $ map (fmap f) chs
    fmap f (File      n l)     = File      n (f l)


instance Foldable FSTree where
    foldMap f (Directory _ l chs) = f l <> foldMap (foldMap f) chs
    foldMap f (File      _ l)     = f l

$(deriveJSON id ''FSTree)


name                   :: FSTree l -> FileName
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


childrenWithNames :: FSTree l -> M.Map FileName (FSTree l)
childrenWithNames = withNames . children


withNames :: [FSTree l] -> M.Map FileName (FSTree l)
withNames = M.fromList . map (name &&& id)


subTree   :: FSTree l -> SubPath-> Maybe (FSTree l)
subTree t = foldl (\mt n -> mt >>= findChild n) (Just t)


findChild   :: FileName -> FSTree l -> Maybe (FSTree l)
findChild n = listToMaybe . filter ((n ==) . name) . children







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
