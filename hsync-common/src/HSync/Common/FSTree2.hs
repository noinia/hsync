module HSync.Common.FSTree2 where

import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO, MonadIO)

import Data.List(break)
import Data.Maybe(catMaybes)
import Data.Text(Text)

import HSync.Common.Types(FileName, SubPath)
import HSync.Common.AtomicIO(exists)


import System.Directory
import System.FilePath (takeFileName, dropTrailingPathSeparator, (</>))
import System.Directory (getDirectoryContents)


import qualified Data.Text as T

--------------------------------------------------------------------------------


data File      fl   = File { fileName  :: FileName
                           , fileLabel :: fl
                           }
                      deriving (Show, Read, Eq)


hasFileName :: File fl -> FileName -> Bool
f `hasFileName` n = (== n) . fileName $ f

data Directory fl dl = Directory { dirName        :: FileName
                                 , dirLabel       :: dl
                                 , subDirectories :: [Directory fl dl]
                                 , files          :: [File fl]
                                 }
                      deriving (Show, Read, Eq)


hasDirName :: Directory fl dl -> FileName -> Bool
d `hasDirName`  n = (== n) . dirName $ d



data FSTree fl dl = F (File fl)
                  | D (Directory fl dl)
                  deriving (Show,Read)




readFSTree :: (Functor m, MonadIO m) => FilePath ->
              (FilePath -> m fl) -> -- ^ how to ocmpute a file label
              (FilePath -> m dl) -> -- ^  how to compute a directory label
              m (Maybe (FSTree fl dl))
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



data Crumb dl unCh ch = Crumb { dirName'    :: FileName
                              , dirLabel'   :: dl
                              , unchanged   :: [unCh]
                              , lefts       :: [ch]
                              , rights      :: [ch]
                              }
                      deriving (Show,Read,Eq)

toDir                      :: Crumb dl unCh ch ->
                              [Directory fl dl] -> [File fl] -> Directory fl dl
toDir (Crumb n dl _ _ _) = Directory n dl



data FSCrumb fl dl = DCrumb (Crumb dl (File fl)         (Directory fl dl))
                   | FCrumb (Crumb dl (Directory fl dl) (File fl))
                   deriving (Show,Read, Eq)

-- | Zipper with additiona lzipper state zs
type FSTreeZipper fl dl zs = (FSTree fl dl, [FSCrumb fl dl], zs)


goUp :: FSTreeZipper fl dl zs -> Maybe (FSTreeZipper fl dl zs)
goUp (F f, (FCrumb c):bs,zs) = let ds = unchanged c
                                   fs = lefts c ++ [f] ++ rights c in
                               Just (D $ toDir c ds fs, bs, zs)
goUp (D d, (DCrumb c):bs,zs) = let fs = unchanged c
                                   ds = lefts c ++ [d] ++ rights c in
                               Just (D $ toDir c ds fs, bs, zs)
goUp (_,[],zs)               = Nothing
goUp _                       = error "goUp: inconsistent zipper"


goToFile :: FSTreeZipper fl dl zs -> FileName -> Maybe (FSTreeZipper fl dl zs)
goToFile (D (Directory n dl ds fs), bs, zs) fn = case break (`hasFileName` fn) fs of
  (_,   [])    -> Nothing
  (lfs, f:rfs) -> Just (F f, FCrumb (Crumb n dl ds lfs rfs):bs,zs)
goToFile _ _                                   = Nothing -- if we are already at a file
                                                         -- we cannot go to a child


goToDir :: FSTreeZipper fl dl zs -> FileName -> Maybe (FSTreeZipper fl dl zs)
goToDir (D (Directory n dl ds fs), bs, zs) dn = case break (`hasDirName` dn) ds of
  (_,   [])    -> Nothing
  (lds, d:rds) -> Just (D d, DCrumb (Crumb n dl fs lds rds):bs,zs)
goToDir _ _                                   = Nothing -- if we are already at a file
                                                         -- we cannot go to a child


goToDirOrFile     :: FSTreeZipper fl dl zs -> FileName -> Maybe (FSTreeZipper fl dl zs)
goToDirOrFile z n = let dz = goToDir z n in case dz of
                      Just _  -> dz
                      Nothing -> goToFile z n


-- | traverse the sub-path
goTo   :: FSTreeZipper fs dl zs -> SubPath -> Maybe (FSTreeZipper fs dl zs)
goTo z = foldl (\z' n -> z' >>= flip goToDirOrFile n) (return z)



-- | If we are curently on a file, get the next file
goToNextFile :: FSTreeZipper fl dl zs -> Maybe (FSTreeZipper fl dl zs)
goToNextFile (F f, (FCrumb c):bs,zs) = case rights c of
  []      -> Nothing
  (f':rs) -> let c' = c { lefts  = lefts c ++ [f]
                        , rights = rs } in
             Just (F f', FCrumb c':bs,zs)
goToNextFile (F _, [], _)            = Nothing
goToNextFile _                       = error "nextFile: inconsistent zipper"
