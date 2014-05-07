{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language FunctionalDependencies #-}
{-# Language MultiParamTypeClasses #-}
module HSync.Common.FSTree( File(..)
                          , Directory(..)
                          , IsFileOrDirectory(..)
                          , Measured(..)

                          -- , FSTree(..)
                          , emptyDirectory
                          , file

                          , findFileAt
                          , findDirectoryAt
                          , findAt

                          , fileExists
                          , directoryExists

                          , addFile
                          , addDirectory

                          , addFileAt
                          , addDirectoryAt

                          , updateFileAt
                          , updateDirectoryAt

                          , readFS
                          , readDirectory

                          -- , labelBottomUp
                          -- , updateLabel

                          , deleteFile
                          , deleteDirectory

                          , deleteFileAt
                          , deleteDirectoryAt

                          -- , prettyPrintTree

                          , andLast
                          ) where


import Control.Applicative((<$>))
import Control.Monad.IO.Class(MonadIO(..))

import Data.Aeson
import Data.Aeson.TH
import Data.Data(Data, Typeable)
import Data.Either(lefts,rights, either)
import Data.Maybe(fromMaybe, catMaybes, isJust)
import Data.Ord(comparing)
import Data.Semigroup
import Data.List.NonEmpty(NonEmpty(..))
import Data.Sequence(Seq, (|>), (<|), ViewL(..), viewl)
import Data.Text(Text)

import HSync.Common.Types(FileName, SubPath)
import HSync.Common.AtomicIO(exists)


import System.Directory(getDirectoryContents)
import System.FilePath (takeFileName, dropTrailingPathSeparator, (</>))

import Data.SafeCopy(base, deriveSafeCopy)

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Text     as T
















--------------------------------------------------------------------------------

class Semigroup m => Measured m a | a -> m where
  measure :: a -> m


--------------------------------------------------------------------------------
-- | A type safe representation of the file system.

-- | A type representing a file with a file label of type
data File m a = File { fileName    :: FileName
                     , fileData    :: a
                     , fileMeasure :: m
                     }
              deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Functor (File m) where
  fmap f (File n x m) = File n (f x) m

$(deriveJSON defaultOptions ''File)
$(deriveSafeCopy 0 'base ''File)

------------------------------

-- | A type representing directories iwth labels l. The subDirectories and
-- files are stored sorted on name.
data Directory m a = Directory { dirName        :: FileName
                               , dirData        :: a
                               , dirMeasure     :: m
                               , subDirectories :: Seq (Directory m a)
                               , files          :: Seq (File      m a)
                             }
                 deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Functor (Directory m) where
  fmap f (Directory n x m sd fs) = Directory n (f x) m sd' fs'
    where
      sd' = fmap (fmap f) sd
      fs' = fmap (fmap f) fs



-- Parse/write Sequences as normal lists
instance ToJSON a => ToJSON (Seq a) where
  toJSON = toJSON . F.foldr (:) []

instance FromJSON a => FromJSON (Seq a) where
  parseJSON = fmap S.fromList . parseJSON

$(deriveJSON defaultOptions ''Directory)
$(deriveSafeCopy 0 'base ''Directory)


--------------------------------------------------------------------------------
-- | Some functions that work on both files and directories

class IsFileOrDirectory c where
  name        :: c m a -> FileName
  dataValue   :: c m a -> a

  measurement :: Measured m a => c m a -> m
  measurement = get measureL

  measureL :: Measured m a => Lens (c m a) m


instance IsFileOrDirectory File where
  name        = fileName
  dataValue   = fileData

  measureL    = ( fileMeasure
                , \m' (File n a m) -> File n a (m <> m')
                )


instance IsFileOrDirectory Directory where
  name        = dirName
  dataValue   = dirData

  measureL = ( dirMeasure
             , \m' (Directory n a m sd fs) -> Directory n a (m <> m') sd fs
             )


--------------------------------------------------------------------------------
-- | Roll out some very basic lenses to modify the directories


type Lens a b = (a -> b, b -> a -> a)

get = fst
set = snd

filesL :: Lens (Directory m a) (Seq (File m a))
filesL = ( files
          ,  \fs' (Directory n a m sd fs) -> Directory n a m sd fs'
          )
dirsL :: Lens (Directory m a) (Seq (Directory m a))
dirsL = ( subDirectories
         ,  \sd' (Directory n a m sd fs) -> Directory n a m sd' fs
         )


--------------------------------------------------------------------------------
-- | Basic Operations on Files and Directories


hasName       :: IsFileOrDirectory t => t m a -> FileName -> Bool
x `hasName` n = (== n) . name $ x


-- | Create a file, the measurement is directly derived from the file data.
file     :: Measured m a => FileName -> a -> File m a
file n x = File n x (measure x)

-- | Create an empty directory
emptyDirectory     :: Measured m a => FileName -> a -> Directory m a
emptyDirectory n x = Directory n x (measure x) mempty mempty

----------------------------------------
-- | Reading from the Filesystem

-- | Read a directory form the filesytem (if it exists). If the path points to
-- a file, we return a Nothing
readDirectory      :: (MonadIO io, Functor io, Measured m a)
                   => FilePath
                   -> (FilePath -> io a)
                   -> io (Maybe (Directory m a))
readDirectory fp f = g <$> readFS fp f
  where
    g (Just (Right d)) = Just d
    g _                = Nothing


-- | Read a file or directory from the file system (if it exists)
readFS      :: (MonadIO io, Functor io, Measured m a)
            => FilePath
           -> (FilePath -> io a)
           -> io (Maybe (Either (File m a)  (Directory m a)))
readFS p f = do
               t <- exists p
               case t of
                 (False, False) -> return Nothing
                 (True,  False) -> f p >>= \x ->
                                   just . Left $ File n x (measure x)
                 (_,     True)  -> do
                                     x   <- f p
                                     ys' <- mapM (flip readFS f) =<< children
                                     let ys = catMaybes ys'
                                         ms = map (either measurement measurement) ys
                                         m = sconcat $ (measure x) :| ms
                                     just . Right $
                                       Directory n x m (dirs' ys) (files' ys)
  where
    just = return . Just

    toSeq :: IsFileOrDirectory t => [t m a] -> Seq (t m a)
    toSeq = S.unstableSortBy (comparing name) . S.fromList

    files' = toSeq . lefts
    dirs'  = toSeq . rights

    n            = T.pack . takeFileName . dropTrailingPathSeparator $ p
    selfOrParent = (`elem` [".",".."])
    children     = (\ns' -> [ p </> n' | n' <- ns',  not $ selfOrParent n' ]
                   ) <$> (liftIO $ getDirectoryContents p)



----------------------------------------
-- | Querying the tree


findFileAt :: SubPath -> Directory m a -> Maybe (File m a)
findFileAt = findAtL filesL


fileExists p = isJust . findFileAt p

findDirectoryAt    :: SubPath -> Directory m a -> Maybe (Directory m a)
findDirectoryAt [] = Just . id    -- If the path is empty do return the main directory
findDirectoryAt sp = findAtL dirsL sp

directoryExists p = isJust . findDirectoryAt p


-- | Find either a file or a directory. If, somehow there are both a directory
-- and a file with this path, preference is given to the file.
findAt      :: SubPath -> Directory m a -> Maybe (Either (File m a) (Directory m a))
findAt sp d = (Right <$> findDirectoryAt sp d) <> (Left <$> findFileAt sp d)


----------------------------------------
-- | Adding files and Directories

-- | Adds a file to the directory
addFile :: Measured m a => File m a -> Directory m a -> Directory m a
addFile = addByName filesL


addDirectory :: Measured m a
             => Directory m a -- ^ The subdirectory to add
             -> Directory m a -- ^ The directory in which we add (the previous argument)
             -> Directory m a
addDirectory = addByName dirsL


-- | addFileAt sp f d, adds file f to the subdirectory indicated by sp.
--
--  prec: the directory indicated by sp exists
addFileAt :: Measured m a => SubPath -> File m a -> Directory m a -> Directory m a
addFileAt = addAt filesL

-- | addDirecotryAt sp d' d adds directory d' to the subdirectory indicated by sp
addDirectoryAt :: Measured m a => SubPath -> Directory m a -> Directory m a -> Directory m a
addDirectoryAt = addAt dirsL

----------------------------------------
-- | Updates

-- | Given a path sp to a file, and a function f, apply f to the file indicated
-- by sp.
updateFileAt    :: Measured m a
                => SubPath      -- ^ The (full) path to the file that we want to update
                -> (File m a -> File m a) -- ^ The update function
                -> Directory m a
                -> Directory m a
updateFileAt sp = let (sp',n) = andLast sp in
                  updateAt filesL sp' n

-- | Update a directory. See updateFileAt
updateDirectoryAt    :: Measured m a
                     => SubPath
                     -> (Directory m a -> Directory m a)
                     -> Directory m a
                     -> Directory m a
updateDirectoryAt sp = let (sp',n) = andLast sp in
                       updateAt dirsL sp' n

----------------------------------------
-- | Deleting files and directories

-- | Given a measurement and a file, delete that file from the directory. The
-- measurement is used when updating the measurement of the directory
deleteFile     :: Measured m a => m -> File m a -> Directory m a -> Directory m a
deleteFile m f = deleteByName filesL (Just m) (name f)

-- | Delete a subdirectory. See deleteFile
deleteDirectory      :: Measured m a
                     => m
                     -> Directory m a -- ^ The directory to delete
                     -> Directory m a -- ^ the directory in which we delete
                     -> Directory m a
deleteDirectory m sd = deleteByName dirsL (Just m) (name sd)


deleteFileAt        :: Measured m a
                    => SubPath -> FileName -> m -> Directory m a -> Directory m a
deleteFileAt sp n m = deleteAt filesL sp n (Just m)

deleteDirectoryAt        :: Measured m a
                         => SubPath -> FileName -> m -> Directory m a -> Directory m a
deleteDirectoryAt sp n m = deleteAt dirsL sp n (Just m)

--------------------------------------------------------------------------------
-- | Medium level functions to manipulate directory structures


-- | Find the 't' (i.e file or directory) at the given path.
--
-- Note: When the path is empty we return Nothing. Even if we are looking
-- for a directory  (i.e. type t is directory)
findAtL            :: IsFileOrDirectory t
                   => Lens (Directory m a) (Seq (t m a))
                   -> SubPath       -- ^ The Path to the directory in which the
                   -- file/dirshould be inserted.
                   -- This path should exist before running this function
                   -> Directory m a
                   -> Maybe (t m a)
findAtL l []     d = Nothing
findAtL l [n]    d = extractByName' l     n d
findAtL l (n:sp) d = extractByName' dirsL n d >>= findAtL l sp


addAt            :: (Measured m a, IsFileOrDirectory t)
                 => Lens (Directory m a) (Seq (t m a))
                 -> SubPath       -- ^ Path to the directory in which the
                                  -- file/dirshould be inserted.
                                  -- This path should exist before running this function
                 -> t m a         -- ^  The file to insersert
                 -> Directory m a
                 -> Directory m a
addAt l []     t = addByName l t
addAt l (n:sp) t = updateByName dirsL (addAt l sp t) n


updateAt               :: (Measured m a, IsFileOrDirectory t)
                       => Lens (Directory m a) (Seq (t m a))
                       -> SubPath       -- ^ Path to the directory containing
                                        -- the file that we want to update.
                                        -- This path should exist before
                                        -- running this function
                       -> FileName      -- ^ The name of the file that we want to update
                       -> (t m a -> t m a)  -- ^ The update function to run on the
                                            -- file
                       -> Directory m a
                       -> Directory m a
updateAt l []      n f = updateByName l f n
updateAt l (dn:sp) n f = updateByName dirsL (updateAt l sp n f) dn


deleteAt               :: (Measured m a, IsFileOrDirectory t)
                       => Lens (Directory m a) (Seq (t m a))
                       -> SubPath    -- ^ Path to the directory containing the
                                     -- thing that we want to delete
                       -> FileName   -- ^ Name of the file that we want to remove
                       -> Maybe m    -- ^ measured value to use at the place of of
                                     -- the deleted file
                       -> Directory m a
                       -> Directory m a
deleteAt l []      n mm = deleteByName l mm n
deleteAt l (dn:sp) n mm = updateByName dirsL (deleteAt l sp n mm) dn

--------------------------------------------------------------------------------
-- | Some low level functons for maniuplating directories. I.e. adding or deleting
-- a file/dir from the list of files/subdirectories


extractByName     :: IsFileOrDirectory t
                  => FileName -> Seq (t m a) -> Maybe (t m a, Seq (t m a))
extractByName n s = let (l,r)  = binarySearch p s
                        p      = (>= n) . name
                    in extractHead r >>= \(x,r') ->
                         if name x == n then Just (x,  l <> r')
                                        else Nothing


-- | Get the item based on a lens
extractByName'       :: IsFileOrDirectory t
                     => Lens (Directory m a) (Seq (t m a))
                     -> FileName
                     -> Directory m a
                     -> Maybe (t m a)
extractByName' l n d = fst <$> (extractByName n . get l $ d)



updateMeasure      :: (IsFileOrDirectory t, IsFileOrDirectory t', Measured m a)
                   => t' m a     -- ^ The thing that provides the updated measure
                   -> t  m a     -- ^ The thing whose measure to update
                   -> t  m a
updateMeasure x' x = set measureL (measurement x') x


-- | Add a thing of type t, (either a file or directory), to the directory.
addByName       :: (Measured m a, IsFileOrDirectory t)
                => Lens (Directory m a) (Seq (t m a))      -- ^ Accessor for
                                                           -- the squence that
                                                           -- we update
                -> t m a -> Directory m a -> Directory m a
addByName l x d = let s  = get l d
                      s' = insertByName x s
                  in updateMeasure x $ set l s' d


deleteByName          :: (Measured m a, IsFileOrDirectory t)
                      => Lens (Directory m a) (Seq (t m a)) -- ^ accessor for the sequence
                      -> Maybe m            -- ^ A New measurement for the deleted file
                      -> FileName           -- ^ The name of the file/dir to delete
                      -> Directory m a
                      -> Directory m a
deleteByName l mm n d = maybe d update . extractByName n $ get l d
  where
    update (_,s) = updateMeasure' $ set l s d

    -- If the measure is not supplied, leave the original measure in tact
    updateMeasure' d' = maybe d' (\m -> set measureL m d) mm




updateByName         :: (Measured m a, IsFileOrDirectory t)
                     => Lens (Directory m a) (Seq (t m a))
                     -> (t m a -> t m a)
                     -> FileName
                     -> Directory m a
                     -> Directory m a
updateByName l f n d = maybe errMsg update . extractByName n $ get l d
  where
    update (x,s) = let x'  = f x
                       -- The file data may have changed, so make sure that the
                       -- measure is also updated:
                       x'' = set measureL (measure $ dataValue x') x'
                       s'  = insertByName x'' s
                   in updateMeasure x'' $ set l s' d

    errMsg = error $ "updateByName: No such file or directory " <> show n


--------------------------------------------------------------------------------
-- | Helper functions for working with (Ordered Sequences)


insertByName :: IsFileOrDirectory t => t m a -> Seq (t m a) -> Seq (t m a)
insertByName = insertSortedBy (comparing name)


insertSorted :: Ord a => a -> Seq a -> Seq a
insertSorted = insertSortedBy compare

insertSortedBy         :: (a -> a -> Ordering) -> a -> Seq a -> Seq a
insertSortedBy ord x s = let (l,r) = binarySearch ((GT /=) . ord x) s
                         in l <> (x <| r)


-- binSearch p s = (a,b)  s.t. the first elem of b is the smallest element
-- that does satisfy the predicate p.
binarySearch     :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
binarySearch p s = case extractMid s of
                     Nothing      -> (s,mempty)
                     Just (l,m,r) -> if p m then
                                       let (ll,lr) = binarySearch p l in
                                       (ll, lr <> (m <| r))
                                     else
                                       let (rl,rr) = binarySearch p r in
                                       ((l |> m) <> rl, rr)


splitHalf   :: Seq a -> (Seq a, Seq a)
splitHalf s = let n = S.length s in
              S.splitAt (n `div` 2) s

extractMid   :: Seq a -> Maybe (Seq a, a, Seq a)
extractMid s = let (l,r) = splitHalf s in
               (\(x,r') -> (l,x,r')) <$> extractHead r


extractHead   :: Seq a -> Maybe (a, Seq a)
extractHead s = case viewl s of
                  EmptyL  -> Nothing
                  x :< xs -> Just (x,xs)




andLast        :: [a] -> ([a],a)
andLast []     = error "andLast: empty list."
andLast [y]    = ([],y)
andLast (x:xs) = let (xs',y) = andLast xs in (x:xs',y)



--------------------------------------------------------------------------------
-- | Testing stuff





--test = S.fromList $ map (flip File ()) ["A","B","C","D","E"]

instance Measured [a] [a] where
  measure = id
