{-# Language TemplateHaskell #-}
module HSync.Common.FSTree( FSTree(..)
                          , name
                          , isRegularFile
                          , isDirectory
                          , label
                          , children

                          , readFSTree'


                          , readFSTree
                          , toFileIdent
                          ) where

import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO, MonadIO(..))


import Data.Aeson.TH

import Data.List(isPrefixOf)
import Data.Text(Text)


import HSync.Common.AtomicIO
import HSync.Common.DateTime(DateTime, modificationTime)



import System.Directory
import System.FilePath (takeFileName, dropTrailingPathSeparator, (</>))
import System.Directory (getDirectoryContents)

import qualified Data.Text as T
import qualified HSync.Common.FileIdent as FI

--------------------------------------------------------------------------------

type Name = Text

data FSTree l = Directory Name l [FSTree l]
              | File      Name l
                deriving (Show,Read,Eq)


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






-- | Read an FSTree from the disk. Use the genLabel function to generate the
-- labels stored in each node
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


readFSTree :: (Functor m, MonadIO m) => FilePath -> m (FSTree DateTime)
readFSTree = readFSTree' modificationTime


toFileIdent                   :: FSTree DateTime -> FI.FileIdent
toFileIdent (Directory _ d _) = FI.Directory d
toFileIdent (File      _ d)   = FI.File d
