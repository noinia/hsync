{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.FSTree.Basic( File(..)
                                , hasFileName

                                , Directory(..)
                                , hasDirName

                                , FSTree(..)
                                , rootFileName
                                , isDir
                                , isFile
                                , label
                                , emptyDirectory

                                , addFile
                                , addSubDir

                                , readFSTree

                                , labelBottomUp
                                , updateLabel

                                , removeFile
                                , removeSubDir

                                , prettyPrintTree
                                ) where


import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO, MonadIO)

import Data.Aeson.TH
import Data.Data(Data, Typeable)

import Data.List(break)
import Data.Maybe(catMaybes, fromJust)
import Data.Text(Text)

import HSync.Common.Types(FileName, SubPath)
import HSync.Common.AtomicIO(exists)


import System.Directory(getDirectoryContents)
import System.FilePath (takeFileName, dropTrailingPathSeparator, (</>))

import Data.SafeCopy(base, deriveSafeCopy)

import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | A type safe representation of the file system.

-- | A type representing a file with a file label of type fl
data File      fl   = File { fileName  :: FileName
                           , fileLabel :: fl
                           }
                      deriving (Show, Read, Eq, Data, Typeable)

instance Functor File where
  fmap f (File n l) = File n (f l)

$(deriveJSON defaultOptions ''File)
$(deriveSafeCopy 0 'base ''File)

------------------------------

-- | A type representing directories. A directory has a label of type dl. The
-- files that it stores have type fl.
data Directory fl dl = Directory { dirName        :: FileName
                                 , dirLabel       :: dl
                                 , subDirectories :: [Directory fl dl]
                                 , files          :: [File fl]
                                 }
                      deriving (Show, Read, Eq, Data, Typeable)

instance Functor (Directory fl) where
  fmap f (Directory n l sd fs) = Directory n (f l) (map (fmap f) sd) fs

$(deriveJSON defaultOptions ''Directory)
$(deriveSafeCopy 0 'base ''Directory)

------------------------------

-- | Data type representing a filesystem tree
data FSTree fl dl = F (File fl)
                  | D (Directory fl dl)
                  deriving (Eq,Show,Read,Data,Typeable)


$(deriveJSON defaultOptions ''FSTree)
$(deriveSafeCopy 0 'base ''FSTree)


------------------------------
-- | Some simple operations on files/directories

-- | Check if the directory has the given name
hasDirName        :: Directory fl dl -> FileName -> Bool
d `hasDirName`  n = (== n) . dirName $ d


-- | Check if the file has a given name
hasFileName       :: File fl -> FileName -> Bool
f `hasFileName` n = (== n) . fileName $ f


-- | Get the filename of the root of the FSTree
rootFileName       :: FSTree fl dl -> FileName
rootFileName (F f) = fileName f
rootFileName (D d) = dirName d

-- | Check if the tree is a directory
isDir       :: FSTree fl dl -> Bool
isDir (D _) = True
isDir _     = False

-- | Check if the tree is a file
isFile :: FSTree fl dl -> Bool
isFile = not . isDir

label       :: FSTree fl dl -> Either fl dl
label (F f) = Left . fileLabel $ f
label (D d) = Right . dirLabel $ d


addFile         :: File fl -> FSTree fl dl -> FSTree fl dl
addFile _ (F _) = error "addFile: cannot add a file to a file"
addFile f (D d) = D $ d { files = f : files d }

addSubDir          :: Directory fl dl -> FSTree fl dl -> FSTree fl dl
addSubDir _  (F _) = error "addSubDir: cannot add a subdirectory to a file"
addSubDir d' (D d) = D $ d { subDirectories = d' : subDirectories d }


emptyDirectory     :: FileName -> dl -> Directory fl dl
emptyDirectory n l = Directory n l [] []

--------------------------------------------------------------------------------
-- | Reading a FSTree from Disk


-- | Reading a FSTree from disk
readFSTree                  :: (Functor m, MonadIO m) =>
                               FilePath -- ^ base dir
                            -> (FilePath -> m fl) -- ^ how to ocmpute a file label
                            -> (FilePath -> m dl)  -- ^ how to compute a dir label
                            -> m (Maybe (FSTree fl dl))
readFSTree baseDir fL fDirL = do
                                t <- exists baseDir
                                case t of
                                  (False, False) -> return Nothing
                                  (True,  False) -> fL baseDir >>=
                                                      return . Just . F . File n
                                  (_,     True)  -> do
                                                      dl      <- fDirL baseDir
                                                      (fs,ds) <- gather <$> chs
                                                      return . Just . D $
                                                        Directory n dl ds fs
  where
    n            = T.pack . takeFileName . dropTrailingPathSeparator $ baseDir
    selfOrParent = (`elem` [".",".."])
    chs          = do
                     ps'  <- liftIO $ getDirectoryContents baseDir
                     let ps = map (baseDir </>) . filter (not . selfOrParent) $ ps'
                     mapM (\bd -> readFSTree bd fL fDirL) ps
    gather       = foldr (\a (fs,ds) -> case a of
                             F f -> (f:fs,ds)
                             D d -> (fs,d:ds)
                         ) ([],[]) . catMaybes

--------------------------------------------------------------------------------
-- | Recursive labelling


labelBottomUp         :: (oldDl -> [dl] -> [fl] -> dl) ->
                         FSTree fl oldDl -> FSTree fl dl
labelBottomUp _ (F t) = F t
labelBottomUp f (D d) = D $ labelBottomUp' f d


labelBottomUp'                          :: (oldDl -> [dl] -> [fl] -> dl) ->
                                           Directory fl oldDl -> Directory fl dl
labelBottomUp' f (Directory n ol sd fs) = Directory n (f ol sdl fsl) sd' fs
  where
    sd' = map (labelBottomUp' f) sd
    sdl = map dirLabel  sd'
    fsl = map fileLabel fs


updateLabel           :: (fl -> fl) -> (dl -> dl) -> FSTree fl dl -> FSTree fl dl
updateLabel f _ (F t) = F $ t { fileLabel = f $ fileLabel t }
updateLabel _ f (D t) = D $ t { dirLabel  = f $ dirLabel t }

--------------------------------------------------------------------------------

-- -- | Remove the file with filename n from the tree. We only remove n if it is
-- removeFile          :: FileName -> FSTree fl dl -> Maybe (FSTree fl dl)
-- removeFile n t@(F f)
--   | fileName f == n = Nothing
--   | otherwise       = Just t
-- removeFile n (D d)  = Just . D $ removeFile' n d

removeFile     :: FileName -> Directory fl dl -> Directory fl dl
removeFile n d = d { files = filter ((/= n) . fileName) . files $ d }


removeSubDir     :: FileName -> Directory fl dl -> Directory fl dl
removeSubDir n d = d { subDirectories = filter ((/= n) . dirName) . subDirectories $ d }


-- removeSubDir'         :: FileName -> FSTree fl dl -> FSTree fl dl
-- removeSubDir' n (D d) = D $ removeSubDir n d
-- removeSubDir' _ t     = t


--------------------------------------------------------------------------------

prettyPrintTree       :: (Show fl, Show dl) => FSTree fl dl -> String
prettyPrintTree (F f) = prettyPrintFile "" f
prettyPrintTree (D d) = unlines $ prettyPrintDir "" d


prettyPrintFile pref (File n l) = pref ++ "+ File "  ++ show n
                                       ++ " label: " ++ show l


prettyPrintDir pref d = let pref' = pref ++ "  " in
                              [ pref ++ "Directory " ++ show (dirName d)
                                     ++ " label = "  ++ show (dirLabel d)
                              ]
                              ++
                              (concatMap (prettyPrintDir pref') . subDirectories $ d)
                              ++
                              (map (prettyPrintFile pref') . files $ d)
