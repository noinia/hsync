{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language FunctionalDependencies #-}
{-# Language MultiParamTypeClasses #-}
module HSync.Common.FSTree( File(..)
                          , Directory(..)
                          , IsFileOrDirectory(..)

                          -- , FSTree(..)
                          , emptyDirectory

                          , addFile
                          , addDirectory

                          , addFileAt
                          , addDirectoryAt

                          -- , readFSTree

                          -- , labelBottomUp
                          -- , updateLabel

                          , deleteFile
                          , deleteDirectory

                          -- , prettyPrintTree
                          ) where


import Control.Applicative((<$>))
import Control.Monad.IO.Class(liftIO, MonadIO)

import Data.Aeson
import Data.Aeson.TH
import Data.Data(Data, Typeable)


import Data.List(break)
import Data.Maybe(catMaybes, fromMaybe)

import Data.Ord(comparing)
import Data.Semigroup
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


-- Parse/write Sequences as normal lists
instance ToJSON a => ToJSON (Seq a) where
  toJSON = toJSON . F.foldr (:) []

instance FromJSON a => FromJSON (Seq a) where
  parseJSON = fmap S.fromList . parseJSON

$(deriveJSON defaultOptions ''Directory)
$(deriveSafeCopy 0 'base ''Directory)

------------------------------
-- | Some simple operations on files/directories

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



hasName       :: IsFileOrDirectory t => t m a -> FileName -> Bool
x `hasName` n = (== n) . name $ x


emptyDirectory     :: Measured m a => FileName -> a -> Directory m a
emptyDirectory n x = Directory n x (measure x) mempty mempty


-- | Adds a file to the directory
addFile :: Measured m a => File m a -> Directory m a -> Directory m a
addFile = addByName filesL


addDirectory :: Measured m a
             => Directory m a -- ^ The subdirectory to add
             -> Directory m a -- ^ The directory in which we add (the previous argument)
             -> Directory m a
addDirectory = addByName dirsL

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



-- | addFileAt sp f d, adds file f to the subdirectory indicated by sp.
--
--  prec: the directory indicated by sp exists
addFileAt :: Measured m a => SubPath -> File m a -> Directory m a -> Directory m a
addFileAt = addAt filesL

-- | addDirecotryAt sp d' d adds directory d' to the subdirectory indicated by sp
addDirectoryAt :: Measured m a => SubPath -> Directory m a -> Directory m a -> Directory m a
addDirectoryAt = addAt dirsL


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
updateDirAt    :: Measured m a
               => SubPath
               -> (Directory m a -> Directory m a)
               -> Directory m a
               -> Directory m a
updateDirAt sp = let (sp',n) = andLast sp in
                  updateAt dirsL sp' n


--------------------------------------------------------------------------------
-- | Medium level functions to manipulate directory structures


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
    update (x,s) = let x' = f x
                       s' = insertByName x' s
                   in updateMeasure x' $ set l s' d

    errMsg = error $ "updateByName: No such file or directory " <> show n


--------------------------------------------------------------------------------
-- | Helper functions for working with (Ordered Sequences)


insertByName :: IsFileOrDirectory t => t m a -> Seq (t m a) -> Seq (t m a)
insertByName = insertSortedBy (comparing name)


insertSortedBy         :: (a -> a -> Ordering) -> a -> Seq a -> Seq a
insertSortedBy ord x s = let (l,r) = binarySearch ((GT /=) . ord x) s
                         in l <> (x <| r)


insertSorted :: Ord a => a -> Seq a -> Seq a
insertSorted = insertSortedBy compare



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
